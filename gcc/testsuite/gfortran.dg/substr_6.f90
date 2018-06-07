! { dg-do run }
! { dg-options "-std=legacy" }
!
! Check that NULs don't mess up constant substring simplification
CHARACTER(5), parameter :: c0(1) = (/ "123" // ACHAR(0) // "5" /)
CHARACTER*5 c(1)
CHARACTER(1), parameter :: c1(5) = (/ "1", "2", "3", ACHAR(0), "5" /)

c = (/ c0(1)(1:5) /)
do i=1,5
   if (c(1)(i:i) /= c1(i)) STOP 2

   ! Make NULs visible (and avoid corrupting text output).
   if (c(1)(i:i) == ACHAR(0)) then
    print "(a,$)", "<NUL>"
  else
    print "(a,$)", c(1)(i:i)
  end if
end do

print *, ""

end
