! { dg-do run }
!
! PR fortran/27997
!
! Empty array constructor with typespec.
!
 integer :: i(3)
 i(3:2) = (/ integer :: /)
 if (len((/ character(5) :: /)) /= 5) call abort()
 if (kind((/ integer(8) :: /)) /= 8) call abort()
end
