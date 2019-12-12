! { dg-do run }
! { dg-additional-sources ISO_Fortran_binding_5.c }
!
! Test fix of PR89385.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
program allocatable_01
  use, intrinsic :: iso_c_binding
  implicit none
  type, bind(c) :: cstruct
     integer(c_int) :: i
     real(c_float) :: r(2)
  end type cstruct
  interface
     subroutine psub(this, that, ierr) bind(c, name='Psub')
       import :: c_float, cstruct, c_int
       real(c_float), allocatable :: this(:,:)
       type(cstruct), allocatable :: that(:)
       integer(c_int), intent(inout) :: ierr
     end subroutine psub
  end interface

  real(c_float), allocatable :: t(:,:)
  type(cstruct), allocatable :: u(:)
  integer(c_int) :: ierr

  allocate(t(3:6,5))
  t = 0.0
  t(4,2) = -2.0
  allocate(u(1), source=[ cstruct( 4, [1.1,2.2] ) ] )
  call psub(t, u, ierr)

  deallocate(t,u)
  if (ierr .ne. 0) stop ierr
end program allocatable_01
