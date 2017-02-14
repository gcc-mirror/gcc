! { dg-do compile }
! PR fortran/69963
subroutine s
  real x ! { dg-error "" }
  implicit character (a) ! { dg-error "IMPLICIT statement at .1. cannot follow data declaration statement at .2." }
  x = 1
  a = 'a'
end subroutine s
