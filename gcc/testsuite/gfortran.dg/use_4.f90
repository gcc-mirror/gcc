! { dg-do compile }
! PR fortran/30973
! Using symbols with the name of the module

module foo
  integer :: i
end module foo

module bar
  integer :: j
end module bar

module test
  use foo, only:
  integer :: foo ! { dg-error "cannot have a type" }
end module test

module test2
  use bar, only: foo => j
  use foo ! ok, unless foo is accessed
end module test2

module test3
  use bar, only: foo => j
  use foo ! ok, unless foo is accessed
  foo = 5 ! { dg-error "is an ambiguous reference to 'j'" }
end module test3

program test_foo
  use foo, only: foo  ! { dg-error "been used as an external module name" }
  use foo, only: i => foo! { dg-error "been used as an external module name" }
  use foo, only: foo => i! { dg-error "been used as an external module name" }
end program
