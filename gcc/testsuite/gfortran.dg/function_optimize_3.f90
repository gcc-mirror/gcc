! { dg-do compile }
! { dg-options "-O" }
! PR 48352 - variable elimination in a DO loop caused segfaults.
! Test case contributed by Joost VandeVondele
program main
  INTEGER, DIMENSION(:), POINTER :: a
  DO I=1,MIN(SIZE(a),SIZE(a))
  ENDDO
END program main
