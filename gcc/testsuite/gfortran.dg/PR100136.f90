! { dg-do run }
! { dg-options "-fcheck=pointer" }
! { dg-shouldfail "Argument not allocated" }
! { dg-output "Fortran runtime error: Allocatable actual argument 'c_init2' is not allocated" }
!
! Tests fix for PR100136
!
! Test cut down from PR58586
!

module test_pr58586_mod
  implicit none

  type :: a
  end type

  type :: c
     type(a), allocatable :: a
  end type

contains

  subroutine add_class_c (d)
    class(c), value :: d
  end subroutine

  class(c) function c_init2()
    allocatable :: c_init2
  end function

end module test_pr58586_mod

program test_pr58586
  use test_pr58586_mod

  ! This needs to execute, to see whether the segfault at runtime is resolved
  call add_class_c(c_init2())

end program
