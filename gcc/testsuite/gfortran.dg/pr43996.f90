! { dg-do compile }
! PR fortran/43996
!
real, parameter :: a(720,360) = spread((/(j, j=1,720) /), dim=2, ncopies=360) ! { dg-error "number of elements" }
real x
x = a(720,360)
end
