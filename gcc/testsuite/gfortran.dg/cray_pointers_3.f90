! { dg-do compile }
program crayerr
  real dpte1(10)
  pointer (iptr1,dpte1) ! { dg-error "fcray-pointer" }
end program crayerr
