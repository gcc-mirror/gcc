! { dg-do compile }
! { dg-additional-options "-std=gnu" }
!
! PR fortran/93289
!
! Contributed by Tobias Burnus

character(len=*), parameter ::  str = "abj", str2 = "1234"
print *, [character(5) :: str, "ab", "hjf333"]
print *, [character(5) :: str, str2]
print *, [str, "ab", "hjf333"]  ! { dg-warning "must have the same length" }
print *, [str, str2]            ! { dg-warning "must have the same length" }
end
