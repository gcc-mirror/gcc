! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! No temporary should be created for this, as a missing stride and
! a stride equal to one should be equal.
program main
  integer a(100)
  a(10:16) = a(11:17)
  a(10:16) = a(11:17:1)
  a(10:16:1) = a(11:17)
  a(10:16:1) = a(11:17:1)
end program main
