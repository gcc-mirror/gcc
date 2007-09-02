! { dg-do run }
! Check that NULs don't mess up constant substring simplification
CHARACTER(5), parameter :: c0(1) = (/ "123" // ACHAR(0) // "5" /)
CHARACTER*5 c(1)
CHARACTER(1), parameter :: c1(5) = (/ "1", "2", "3", ACHAR(0), "5" /)

c = c0(1)(-5:-8)
if (c(1) /= "     ") call abort()
c = (/ c0(1)(1:5) /)
do i=1,5
   if (c(1)(i:i) /= c1(i)) call abort()
end do
print *, c(1)
end
