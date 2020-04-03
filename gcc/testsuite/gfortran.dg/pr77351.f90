! { dg-do compile }
!
! PR93835 resulted in different but valid error message
program p
   integer :: z(4) = [1, 2, 3, 4]
   print *, any(shape(z) /= [4,1])  ! { dg-error "Shapes for operands at .1. and .2. are not conformable" }
end

