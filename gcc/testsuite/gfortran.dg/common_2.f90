! { dg-do run }
! The equivalence was causing us to miss out c when laying out the common
! block.
program common_2
  common /block/ a, b, c, d
  integer a, b, c, d, n
  dimension n(4)
  equivalence (a, n(1))
  equivalence (c, n(3))
  a = 1
  b = 2
  c = 3
  d = 4
  if (any (n .ne. (/1, 2, 3, 4/))) STOP 1
end program
