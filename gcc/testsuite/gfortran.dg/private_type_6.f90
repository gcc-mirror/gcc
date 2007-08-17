! { dg-do compile }
! PR fortran/32460
!
module foomod
  implicit none
  type :: footype
    private
    integer :: dummy
  end type footype
  TYPE :: bartype
    integer :: dummy
    integer, private :: dummy2
  end type bartype
end module foomod

program foo_test
  USE foomod
  implicit none
  TYPE(footype) :: foo
  TYPE(bartype) :: foo2
  foo  = footype(1) ! { dg-error "has PRIVATE components" }
  foo2 = bartype(1,2) ! { dg-error "has PRIVATE components" }
  foo2%dummy2 = 5 ! { dg-error "is a PRIVATE component" }
end program foo_test
! { dg-final { cleanup-modules "foomod" } }
