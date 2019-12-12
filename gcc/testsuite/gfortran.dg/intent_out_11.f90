! { dg-do compile }
! { dg-options "-cpp -fcoarray=lib" }
! PR 87397 - this used to generate an ICE.

! Coarray Distributed Transpose Test
!
! Copyright (c) 2012-2014, Sourcery, Inc.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!     * Neither the name of the Sourcery, Inc., nor the
!       names of its contributors may be used to endorse or promote products
!       derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL SOURCERY, INC., BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!
! Robodoc header:
!****m* dist_transpose/run_size
! NAME
!   run_size
!  SYNOPSIS
!   Encapsulate problem state, wall-clock timer interface, integer broadcasts, and a data copy.
!******
!==================  test transposes with integer x,y,z values  ===============================
module run_size
    use iso_fortran_env
    implicit none

    integer(int64), codimension[*] :: nx, ny, nz
    integer(int64), codimension[*] :: my, mx, first_y, last_y, first_x, last_x
    integer(int64) :: my_node, num_nodes
    real(real64), codimension[*] :: tran_time


contains

!****s* run_size/broadcast_int
! NAME
!   broadcast_int
!  SYNOPSIS
!   Broadcast a scalar coarray integer from image 1 to all other images.
!******
    subroutine broadcast_int( variable )
        integer(int64), codimension[*] :: variable
        integer(int64) :: i
        if( my_node == 1 ) then
            do i = 2, num_nodes;    variable[i] = variable;      end do
        end if
    end subroutine broadcast_int

subroutine copy3( A,B, n1, sA1, sB1, n2, sA2, sB2, n3, sA3, sB3 )
  implicit none
  complex, intent(in)  :: A(0:*)
  complex, intent(out) :: B(0:*)
  integer(int64), intent(in) :: n1, sA1, sB1
  integer(int64), intent(in) :: n2, sA2, sB2
  integer(int64), intent(in) :: n3, sA3, sB3
  integer(int64) i,j,k

  do k=0,n3-1
     do j=0,n2-1
        do i=0,n1-1
           B(i*sB1+j*sB2+k*sB3) = A(i*sA1+j*sA2+k*sA3)
        end do
     end do
  end do
end subroutine copy3

end module run_size

!****e* dist_transpose/coarray_distributed_transpose
! NAME
!   coarray_distributed_transpose
! SYNOPSIS
!   This program tests the transpose routines used in Fourier-spectral simulations of homogeneous turbulence.
!   The data is presented to the physics routines as groups of y-z or x-z planes distributed among the images.
!   The (out-of-place) transpose routines do the x <--> y transposes required and consist of transposes within
!   data blocks (intra-image) and a transpose of the distribution of these blocks among the images (inter-image).
!
!   Two methods are tested here:
!   RECEIVE: receive block from other image and transpose it
!   SEND:    transpose block and send it to other image
!
!   This code is the coarray analog of mpi_distributed_transpose.
!******

program coarray_distributed_transpose
  !(***********************************************************************************************************
  !                   m a i n   p r o g r a m
  !***********************************************************************************************************)
      use run_size
      implicit none

      complex, allocatable ::  u(:,:,:,:)[:]    ! u(nz,4,first_x:last_x,ny)[*]    !(*-- ny = my * num_nodes --*)
      complex, allocatable ::  ur(:,:,:,:)[:]   !ur(nz,4,first_y:last_y,nx/2)[*]  !(*-- nx/2 = mx * num_nodes --*)
      complex, allocatable :: bufr_X_Y(:,:,:,:)
      complex, allocatable :: bufr_Y_X(:,:,:,:)
      integer(int64) :: x, y, z, msg_size, iter

      num_nodes = num_images()
      my_node = this_image()

      if( my_node == 1 ) then
           !write(6,*) "nx,ny,nz : ";      read(5,*) nx, ny, nz
            nx=32; ny=32; nz=32
            call broadcast_int( nx );        call broadcast_int( ny );        call broadcast_int( nz );
       end if
      sync all  !-- other nodes wait for broadcast!


      if ( mod(ny,num_nodes) == 0)  then;   my = ny / num_nodes
                                    else;   write(6,*) "node ", my_node, " ny not multiple of num_nodes";     error stop
      end if

      if ( mod(nx/2,num_nodes) == 0)  then;   mx = nx/2 / num_nodes
                                    else;   write(6,*) "node ", my_node, "nx/2 not multiple of num_nodes";     error stop
      end if

      first_y = (my_node-1)*my + 1;   last_y  = (my_node-1)*my + my
      first_x = (my_node-1)*mx + 1;   last_x  = (my_node-1)*mx + mx

      allocate (  u(nz , 4 , first_x:last_x , ny)  [*] )   !(*-- y-z planes --*)
      allocate ( ur(nz , 4 , first_y:last_y , nx/2)[*] )   !(*-- x-z planes --*)
      allocate ( bufr_X_Y(nz,4,mx,my) )
      allocate ( bufr_Y_X(nz,4,my,mx) )

      msg_size = nz*4*mx*my     !-- message size (complex data items)

!---------  initialize data u (mx y-z planes per image) ----------

        do x = first_x, last_x
            do y = 1, ny
                do z = 1, nz
                    u(z,1,x,y) = x
                    u(z,2,x,y) = y
                    u(z,3,x,y) = z
                end do
            end do
        end do

    tran_time = 0
    do iter = 1, 2  !--- 2 transform pairs per second-order time step

!---------  transpose data u -> ur (mx y-z planes to my x-z planes per image)  --------

      ur = 0

      call transpose_X_Y

!--------- test data ur (my x-z planes per image) ----------

        do x = 1, nx/2
            do y = first_y, last_y
                do z = 1, nz
                    if ( real(ur(z,1,y,x)) /= x .or. real(ur(z,2,y,x)) /= y .or. real(ur(z,3,y,x)) /= z )then
                        write(6,fmt="(A,i3,3(6X,A,f7.3,i4))") "transpose_X_Y failed:  image ", my_node &
                            , " X ",real(ur(z,1,y,x)),x, "  Y ",real(ur(z,2,y,x)),y, "  Z ", real(ur(z,3,y,x)),z
                        stop
                    end if
                end do
            end do
        end do

!---------  transpose data ur -> u (my x-z planes to mx y-z planes per image)  --------

      u = 0
      call transpose_Y_X

!--------- test data u (mx y-z planes per image) ----------

        do x = first_x, last_x
            do y = 1, ny
                do z = 1, nz
                    if ( real(u(z,1,x,y)) /= x .or. real(u(z,2,x,y)) /= y .or. real(u(z,3,x,y)) /= z )then
                        write(6,fmt="(A,i3,3(6X,A,f7.3,i4))") "transpose_Y_X failed:  image ", my_node &
                            , " X ",real(u(z,1,x,y)),x, "  Y ",real(u(z,2,x,y)),y, "  Z ", real(u(z,3,x,y)),z
                        stop
                    end if
                end do
            end do
        end do
    end do

        sync all
        if( my_node == 1 )  write(6,fmt="(A,f8.3)")  "test passed:  tran_time ", tran_time

    deallocate ( bufr_X_Y );    deallocate ( bufr_Y_X )

!=========================   end of main executable  =============================

contains

!-------------   out-of-place transpose data_s --> data_r  ----------------------------

 subroutine transpose_X_Y

    use run_size
    implicit none

    integer(int64) :: i,stage
    real(real64) :: tmp

    sync all   !--  wait for other nodes to finish compute
    call cpu_time(tmp)
    tran_time = tran_time - tmp

    call copy3 (    u(1,1,first_x,1+(my_node-1)*my) &                   !-- intra-node transpose
                ,  ur(1,1,first_y,1+(my_node-1)*mx) &                   !-- no inter-node transpose needed
                ,   nz*3, 1_8, 1_8        &                                 !-- note: only 3 of 4 words needed
                ,   mx, nz*4, nz*4*my &
                ,   my, nz*4*mx, nz*4 )

#define RECEIVE
#ifdef RECEIVE

    do stage = 1, num_nodes-1
        i = 1 + mod( my_node-1+stage, num_nodes )
        bufr_X_Y(:,:,:,:) = u(:,:,:,1+(my_node-1)*my:my_node*my)[i]         !-- inter-node transpose to buffer
        call copy3 ( bufr_X_Y, ur(1,1,first_y,1+(i-1)*mx)  &                !-- intra-node transpose from buffer
                        ,   nz*3, 1_8, 1_8        &                             !-- note: only 3 of 4 words needed
                        ,   mx, nz*4, nz*4*my &
                        ,   my, nz*4*mx, nz*4 )
    end do

#else

    do stage = 1, num_nodes-1
        i = 1 + mod( my_node-1+stage, num_nodes )
        call  copy3 ( u(1,1,first_x,1+(i-1)*my), bufr_Y_X   &        !-- intra-node transpose to buffer
                    ,   nz*3, 1_8, 1_8        &
                    ,   mx, nz*4, nz*4*my &
                    ,   my, nz*4*mx, nz*4 )
        ur(:,:,:,1+(my_node-1)*mx:my_node*mx)[i] = bufr_Y_X(:,:,:,:)        !-- inter-node transpose from buffer
    end do

#endif

    sync all     !--  wait for other nodes to finish transpose
    call cpu_time(tmp)
    tran_time = tran_time + tmp

 end  subroutine transpose_X_Y

!-------------   out-of-place transpose data_r --> data_s  ----------------------------

subroutine transpose_Y_X
    use run_size
    implicit none

    integer(int64) :: i, stage
    real(real64) :: tmp

    sync all   !--  wait for other nodes to finish compute
    call cpu_time(tmp)
    tran_time = tran_time - tmp

    call copy3 (   ur(1,1,first_y,1+(my_node-1)*mx) &                   !-- intra-node transpose
                ,   u(1,1,first_x,1+(my_node-1)*my) &                   !-- no inter-node transpose needed
                ,   nz*4, 1_8, 1_8        &                                 !-- note: all 4 words needed
                ,   my, nz*4, nz*4*mx &
                ,   mx, nz*4*my, nz*4 )

#define RECEIVE
#ifdef RECEIVE

    do stage = 1, num_nodes-1
        i = 1 + mod( my_node-1+stage, num_nodes )
        bufr_Y_X(:,:,:,:) = ur(:,:,:,1+(my_node-1)*mx:my_node*mx)[i]        !-- inter-node transpose to buffer
        call copy3 ( bufr_Y_X, u(1,1,first_x,1+(i-1)*my)  &                 !-- intra-node transpose from buffer
                    ,   nz*4, 1_8, 1_8        &
                    ,   my, nz*4, nz*4*mx &
                    ,   mx, nz*4*my, nz*4 )
    end do

#else

    do stage = 1, num_nodes-1
        i = 1 + mod( my_node-1+stage, num_nodes )
        call copy3 ( ur(1,1,first_y,1+(i-1)*mx), bufr_X_Y  &                 !-- intra-node transpose from buffer
                    ,   nz*4, 1_8, 1_8        &
                    ,   my, nz*4, nz*4*mx &
                    ,   mx, nz*4*my, nz*4 )
        u(:,:,:,1+(my_node-1)*my:my_node*my)[i] = bufr_X_Y(:,:,:,:)        !-- inter-node transpose from buffer
    end do

#endif

    sync all     !--  wait for other nodes to finish transpose
    call cpu_time(tmp)
    tran_time = tran_time + tmp

 end  subroutine transpose_Y_X


end program coarray_distributed_transpose
