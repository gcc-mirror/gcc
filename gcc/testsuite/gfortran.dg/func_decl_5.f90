! { dg-do compile }
! { dg-options "-c" }
!
! Functions shall not have an initializer.
!
! Some tests were moved from func_decl_4.f90 to here.
!

function f1()                      ! { dg-error "cannot have an initializer" }
  integer :: f1 = 42
end function

function f2() RESULT (r)           ! { dg-error "cannot have an initializer" }
  integer :: r = 42
end function
