! { dg-do run { target c99_runtime } }
! { dg-additional-sources ISO_Fortran_binding_7.c }
!
! Test the fix for PR89841.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
program assumed_shape_01
  use, intrinsic :: iso_c_binding
  implicit none
  type, bind(c) :: cstruct
     integer(c_int) :: i
     real(c_float) :: r(2)
  end type cstruct
  interface
     function psub(this, that, case) bind(c, name='Psuba') result(status)
       import :: c_float, c_int, cstruct
       real(c_float) :: this(:,:)
       type(cstruct) :: that(:)
       integer(c_int), value :: case
       integer(c_int) :: status
     end function psub
  end interface

  real(c_float) :: t(3,7)
  type(cstruct), pointer :: u(:)
  type(cstruct), allocatable :: v(:)
  integer(c_int) :: st

  allocate(u(1), source=[cstruct( 4, [1.1,2.2] ) ])
  allocate(v(1), source=[cstruct( 4, [1.1,2.2] ) ])
  t = 0.0
  t(3,2) = -2.0
  st = psub(t, u, 1)
  if (st .ne. 0) stop 1
  st = psub(t, v, 2)
  if (st .ne. 0) stop 2
  deallocate (u)
  deallocate (v)

end program assumed_shape_01

