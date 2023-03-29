! { dg-do compile }
! { dg-options "-Winteger-division" }
! PR fortran/108592 - warn only once for truncation of integer division

program foo
  if (8 < (20/9)) stop 1 ! { dg-bogus "Integer division.*Integer division" }
! { dg-message "Integer division truncated" "" { target *-*-* } .-1 }
end program
