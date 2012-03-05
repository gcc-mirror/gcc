! { dg-do compile }
! { dg-options "-Wc-binding-type" }
module test
use iso_c_binding, only: c_int
    type, bind(c) ::  foo
      integer :: p ! { dg-warning "may not be C interoperable" }
    end type
    type(foo), bind(c) :: cp
end module test

! { dg-final { cleanup-modules "test" } }
