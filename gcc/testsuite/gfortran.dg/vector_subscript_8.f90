! { dg-do run }
!
! PR fortran/102043
! The middle-end used to conclude from array indexing that the index
! should be non-negative and thus that array accesses to reversed arrays
! (i.e. with negative strides) only access the last element of the array,
! as the access involves a pointer to array that is initialized to point
! to the last element in the case of a reversed array.

program main
   integer, dimension (4) :: idx, a, b
   a = (/ 11, 13, 17, 19 /)
   idx = (/ 1, 2, 3, 4 /)
   a(idx(4:1:-1)) = idx
   if (a(1).ne.4) STOP 1
end program main
