! { dg-do compile }
!
! PR fortran/29876 ICE on bad operator in ONLY clause of USE statement
! Testcase contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
module foo
end module foo

program test
  use foo, only : operator(.none.)           ! { dg-error "not found in module" }
  end program test
