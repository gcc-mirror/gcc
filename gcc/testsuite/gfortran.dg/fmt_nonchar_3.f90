! { dg-do compile }
! PR fortran/101084

program p
  integer, parameter :: a(0) = 1
  print int(a) ! { dg-error "Non-character non-Hollerith in FORMAT tag" }
end
