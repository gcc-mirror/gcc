! { dg-do run }
! { dg-options "-fcheck=all" }
! { dg-shouldfail "above upper bound" }
!
! PR fortran/92050
!
!
module buggy
  implicit none (type, external)

  type :: par
  contains
    procedure, public :: fun => fun_par
  end type par

  type comp
    class(par), allocatable :: p
  end type comp

  type foo
    type(comp), allocatable :: m(:)
  end type foo

contains

  function fun_par(this)
    class(par) :: this
    integer    :: fun_par(1)
    fun_par = 42
  end function fun_par

  subroutine update_foo(this)
    class(foo) :: this
    write(*,*) this%m(1)%p%fun()
  end subroutine update_foo

  subroutine bad_update_foo(this)
    class(foo) :: this
    write(*,*) this%m(2)%p%fun()
  end subroutine bad_update_foo
end module buggy

program main
  use buggy
  implicit none (type, external)
  type(foo) :: x
  allocate(x%m(1))
  allocate(x%m(1)%p)
  call update_foo(x)
  call bad_update_foo(x)
end program main

! { dg-output "At line 39 of file .*pr92050.f90.*Fortran runtime error: Index '2' of dimension 1 of array 'this%m' above upper bound of 1" }
