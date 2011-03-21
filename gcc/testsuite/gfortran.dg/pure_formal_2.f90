! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/47550
! Follow up to: PR fortran/47507
!
! PURE procedures: Allow arguments w/o INTENT if they are VALUE
!

pure function f(x) ! { dg-error "Fortran 2008: Argument 'x' of pure function" }
  real, VALUE :: x
  real :: f
  f = sin(x)
end function f

pure subroutine sub(x) ! { dg-error "Fortran 2008: Argument 'x' of pure subroutine" }
  real, VALUE :: x
end subroutine sub
