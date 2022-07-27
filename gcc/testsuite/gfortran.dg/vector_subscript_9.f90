! { dg-do run }
!
! PR fortran/102043
! The middle-end used to conclude from array indexing that the index
! should be non-negative and thus that array accesses to reversed arrays
! (i.e. with negative strides) only access the last element of the array,
! as the access involves a pointer to array that is initialized to point
! to the last element in the case of a reversed array.

program main
   integer, dimension (2) :: idx, a, b
   a = (/ 3, 4 /)
   idx = (/ 1, 2 /)
   call check_values(a(idx(2:1:-1)), (/ 4, 3 /))
contains
   subroutine check_values(values, expected)
      integer, dimension(:) :: values, expected
      if (size(values) /= size(expected)) stop 1
      if (any(values /= expected)) stop 2
   end subroutine check_values
end program main
