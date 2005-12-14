! { dg-do compile }
! PR fortran/25078
! An equivalence statement requires two or more objcets.
program a
  real x
  equivalence(x) ! { dg-error "two or more objects" }
end program a
