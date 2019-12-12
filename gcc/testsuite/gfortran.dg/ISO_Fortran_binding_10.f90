! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_10.c }
!
! Test the fix of PR89843.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_section_01
  use, intrinsic :: iso_c_binding
  implicit none
  interface
     subroutine si(this, flag, status) bind(c)
       import :: c_float, c_int
       real(c_float) :: this(:,:)
       integer(c_int), value :: flag
       integer(c_int) :: status
     end subroutine si
  end interface
contains
  subroutine sa(this, flag, status) bind(c)
    real(c_float) :: this(:)
    integer(c_int), value :: flag
    integer(c_int) :: status

    status = 0

    select case (flag)
    case (0)
       if (is_contiguous(this)) then
          write(*,*) 'FAIL 1:'
          status = status + 1
       end if
       if (size(this,1) /= 3) then
          write(*,*) 'FAIL 2:',size(this)
          status = status + 1
          goto 10
       end if
       if (maxval(abs(this - [ 1.0, 3.0, 5.0 ])) > 1.0e-6) then
          write(*,*) 'FAIL 3:',abs(this)
          status = status + 1
       end if
  10   continue
   case (1)
      if (size(this,1) /= 3) then
          write(*,*) 'FAIL 4:',size(this)
          status = status + 1
          goto 20
       end if
       if (maxval(abs(this - [ 11.0, 12.0, 13.0 ])) > 1.0e-6) then
          write(*,*) 'FAIL 5:',this
          status = status + 1
       end if
  20   continue
   case (2)
      if (size(this,1) /= 4) then
          write(*,*) 'FAIL 6:',size(this)
          status = status + 1
          goto 30
       end if
      if (maxval(abs(this - [ 2.0, 7.0, 12.0, 17.0 ])) > 1.0e-6) then
          write(*,*) 'FAIL 7:',this
          status = status + 1
       end if
  30   continue
    end select

!    if (status == 0) then
!       write(*,*) 'OK'
!    end if
  end subroutine sa
end module mod_section_01

program section_01
  use mod_section_01
  implicit none
  real(c_float) :: v(5,4)
  integer :: i
  integer :: status

  v = reshape( [ (real(i), i = 1, 20) ], [ 5, 4 ] )
  call si(v, 0, status)
  if (status .ne. 0) stop 1

  call sa(v(1:5:2, 1), 0, status)
  if (status .ne. 0) stop 2

  call si(v, 1, status)
  if (status .ne. 0) stop 3

  call sa(v(1:3, 3), 1, status)
  if (status .ne. 0) stop 4

  call si(v, 2, status)
  if (status .ne. 0) stop 5

  call sa(v(2,1:4), 2, status)
  if (status .ne. 0) stop 6

end program section_01
