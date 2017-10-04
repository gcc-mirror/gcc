! { dg-do  run }
! PR 66328 - this used to give a wrong value for integer values for DATA
program main
  TYPE t
    REAL r
  END TYPE t
  TYPE (t) e1, e2
  
  DATA e1 / t(1) /
  DATA e2 / t(1.0) /
  if (abs(e1%r - 1.0) > 1e-6) call abort
  if (abs(e2%r - 1.0) > 1e-6) call abort
END
