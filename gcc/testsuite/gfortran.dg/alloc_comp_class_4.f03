! { dg-do run }
! { dg-options "-Wreturn-type" }
!
! Check that pr58586 is fixed now.
! Based on a contribution by Vladimir Fuka
! Contibuted by Andre Vehreschild

module test_pr58586_mod
  implicit none

  type :: a
  end type

  type :: c
     type(a), allocatable :: a
  end type

  type :: d
  contains
     procedure :: init => d_init
  end type

  type, extends(d) :: e
  contains
     procedure :: init => e_init
  end type

  type :: b
     integer, allocatable :: a
  end type

  type t
    integer :: i = 5
  end type

contains

  subroutine add (d)
    type(b), value :: d
  end subroutine

  subroutine add_c (d)
    type(c), value :: d
  end subroutine

  subroutine add_class_c (d)
    class(c), value :: d
  end subroutine

  subroutine add_t (d)
    type(t), value :: d
  end subroutine

  type(c) function c_init()
  end function

  class(c) function c_init2() ! { dg-warning "not set" }
    allocatable :: c_init2
  end function

  type(c) function d_init(this)
    class(d) :: this
  end function

  type(c) function e_init(this)
    class(e) :: this
    allocate (e_init%a)
  end function

  type(t) function t_init() ! { dg-warning "not set" }
    allocatable :: t_init
  end function

  type(t) function static_t_init()
  end function
end module test_pr58586_mod

program test_pr58586
  use test_pr58586_mod

  class(d), allocatable :: od
  class(e), allocatable :: oe
  type(t), allocatable :: temp

  ! These two are merely to check, if compilation works
  call add(b())
  call add(b(null()))

  ! This needs to execute, to see whether the segfault at runtime is resolved
  call add_c(c_init())
  call add_class_c(c_init2())

  call add_t(static_t_init())
  ! temp = t_init() ! <-- This derefs a null-pointer currently
  ! Filed as pr66775
  if (allocated (temp)) STOP 1

  allocate(od)
  call add_c(od%init())
  deallocate(od)
  allocate(oe)
  call add_c(oe%init())
  deallocate(oe)
end program
