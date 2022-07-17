! { dg-do run }
!
! PR fortran/102043
! The middle-end used to conclude from array indexing that the index
! should be non-negative and thus that array accesses to reversed arrays
! (i.e. with negative strides) only access the last element of the array,
! as the access involves a pointer to array that is initialized to point
! to the last element in the case of a reversed array.

program main
   implicit none
   integer :: a(3, 3)
   integer :: i
   a = 0
   call s(a(3:1:-1,:))
   if (any(a(:,1) /= (/  7,  5,  3 /))) stop 1
   if (any(a(:,2) /= (/ 17, 13, 11 /))) stop 2
   if (any(a(:,3) /= (/ 29, 23, 19 /))) stop 3
contains
  subroutine s(b)
    implicit none
    integer, dimension(:,:) :: b
    b = reshape((/ 3, 5, 7, 11, 13, 17, 19, 23, 29 /), (/ 3, 3 /)) 
  end subroutine s
end program main
