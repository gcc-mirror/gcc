! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in return value of PACK intrinsic; is 4, should be 5" }
! PR 30814 - a bounds error with pack was not caught.
program main
  integer :: a(2,2), b(5)
  a = reshape((/ 1, -1, 1, -1 /), shape(a))
  b = pack(a, a /= 0)
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of PACK intrinsic; is 4, should be 5" }
