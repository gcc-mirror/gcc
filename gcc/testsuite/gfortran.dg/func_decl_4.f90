! { dg-do compile }
! { dg-options "-c" }
!
! Functions shall not have an initializer.
!
! Due to -fwhole-file, the function declaration
! warnings come before the init warnings; thus
! the warning for the WRONG lines have been moved to
! func_decl_5.f90
!

function f1()
  integer :: f1 = 42 ! WRONG, see  func_decl_5.f90
end function

function f2() RESULT (r)
  integer :: r = 42 ! WRONG, see func_decl_5.f90
end function

function f3() RESULT (f3)          ! { dg-error "must be different than function name" }
  integer :: f3 = 42
end function                       ! { dg-error "Expecting END PROGRAM" }
! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
