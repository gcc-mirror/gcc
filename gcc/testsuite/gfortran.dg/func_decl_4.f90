! { dg-do compile }
! { dg-options "-c" }
!
! Functions shall not have an initializer.
!

function f1()                      ! { dg-error "cannot have an initializer" }
  integer :: f1 = 42
end function

function f2() RESULT (r)           ! { dg-error "cannot have an initializer" }
  integer :: r = 42
end function

function f3() RESULT (f3)          ! { dg-error "must be different than function name" }
  integer :: f3 = 42
end function                       ! { dg-error "Expecting END PROGRAM" }
! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
