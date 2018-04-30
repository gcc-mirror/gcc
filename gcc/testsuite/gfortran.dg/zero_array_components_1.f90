! { dg-do run }
! Tests the fix for PR31620, in which zeroing the component a for the array,
! would zero all the components of the array.
!
! David Ham <David@ham.dropbear.id.au>
!
program test_assign
  type my_type
     integer :: a
     integer :: b
  end type my_type
  type(my_type), dimension(1) :: mine        ! note that MINE is an array
  mine%b=4
  mine%a=1
  mine%a=0
  if (any (mine%b .ne. 4)) STOP 1
end program test_assign
