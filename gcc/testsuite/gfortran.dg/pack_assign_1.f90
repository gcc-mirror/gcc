! { dg-do "compile" }
! PR32890 - compile-time checks for assigments

INTEGER :: it, neighbrs(42)  ! anything but 30

neighbrs = PACK((/ (it, it=1,30) /), (/ (it, it=1,30) /) < 3, (/ (0,it=1,30) /) )  ! { dg-error "Different shape" }

END
