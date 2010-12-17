! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! PR 44235
! No temporary should be created for this, as the upper bounds
! are effectively identical.
program main
  real a(10)
  a = 0.
  a(1:10:4) = a(1:9:4)
end program main
