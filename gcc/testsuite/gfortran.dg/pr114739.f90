! { dg-do compile }
! The fix here was triggered by an ICE prior to r14-9489-g3fd46d859cda10
! Before that gfortran gave an incorrect "no implicit type" error for all
! three statements.
program main
  implicit complex(z)
  implicit character(c)
  z2%re = 1.
  z2%im = 2.
  print *, z2, c%kind
end
