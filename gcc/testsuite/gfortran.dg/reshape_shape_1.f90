! { dg-do compile }
!
! PR fortran/37203 - check RESHAPE arguments
!

  integer, dimension(6) :: source1 = (/ 1, 2, 3, 4, 5, 6 /)
  integer, dimension(2) :: pad1 = (/ 0, 0/)
  integer, dimension(2) :: t(2,5)
  integer :: i

  t = reshape(source1, SHAPE(0), pad1, (/2, 1/))      ! { dg-error "is empty" }
  t = reshape(source1, (/(i,i=1,32)/), pad1, (/2, 1/))    ! { dg-error "has more than" }
  t = reshape(source1, (/ 2, -5/), pad1, (/2, 1/))    ! { dg-error "negative element" }
end
