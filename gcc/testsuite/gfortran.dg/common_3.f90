! { dg-do compile }
! Check that equivalences match common block layout.
program common_3
  common /block/ a, b, c, d ! { dg-error "not match ordering" "" }
  integer a, b, c, d, n
  dimension n(4)
  equivalence (a, n(1))
  equivalence (c, n(4))
end program
