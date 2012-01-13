! { dg-do run }
! PR48351 - automatic (re)allocation of allocatable components of class objects
!
! Contributed by Nasser M. Abbasi on comp.lang.fortran
!
module foo
  implicit none
  type :: foo_t
    private
    real, allocatable :: u(:)
  contains
    procedure :: make
    procedure :: disp
  end type foo_t
contains
  subroutine make(this,u)
    implicit none
    class(foo_t) :: this
    real, intent(in) :: u(:)
    this%u = u(int (u))       ! The failure to allocate occurred here.
    if (.not.allocated (this%u)) call abort
  end subroutine make
  function disp(this)
    implicit none
    class(foo_t) :: this
    real, allocatable :: disp (:)
    if (allocated (this%u)) disp = this%u
  end function
end module foo

program main2
  use foo
  implicit none
  type(foo_t) :: o
  real, allocatable :: u(:)
  u=real ([3,2,1,4])
  call o%make(u)
  if (any (int (o%disp()) .ne. [1,2,3,4])) call abort
  u=real ([2,1])
  call o%make(u)
  if (any (int (o%disp()) .ne. [1,2])) call abort
end program main2
! { dg-final { cleanup-modules "foo" } }

