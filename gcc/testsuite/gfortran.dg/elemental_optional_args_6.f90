! { dg-do run }
! { dg-options "-Wpedantic" }
!
! PR fortran/53692
!
! Check that the nonabsent arrary is used for scalarization:
! Either the NONOPTIONAL one or, if there are none, any array.
!
! Based on a program by Daniel C Chen
!
Program main
  implicit none
  integer :: arr1(2), arr2(2)
  arr1 = [ 1, 2 ]
  arr2 = [ 1, 2 ]
  call sub1 (arg2=arr2)

  call two ()
contains
   subroutine sub1 (arg1, arg2)
      integer, optional :: arg1(:)
      integer :: arg2(:)
!      print *, fun1 (arg1, arg2)
      if (size (fun1 (arg1, arg2)) /= 2) STOP 1 ! { dg-warning "is an array and OPTIONAL" }
      if (any (fun1 (arg1, arg2) /= [1,2])) STOP 2 ! { dg-warning "is an array and OPTIONAL" }
   end subroutine

   elemental function fun1 (arg1, arg2)
      integer,intent(in), optional :: arg1
      integer,intent(in)           :: arg2
      integer                      :: fun1
      fun1 = arg2
   end function
end program

subroutine two ()
  implicit none
  integer :: arr1(2), arr2(2)
  arr1 = [ 1, 2 ]
  arr2 = [ 1, 2 ]
  call sub2 (arr1, arg2=arr2)
contains
   subroutine sub2 (arg1, arg2)
      integer, optional :: arg1(:)
      integer, optional :: arg2(:)
!      print *, fun2 (arg1, arg2)
      if (size (fun2 (arg1, arg2)) /= 2) STOP 3 ! { dg-warning "is an array and OPTIONAL" }
      if (any (fun2 (arg1, arg2) /= [1,2])) STOP 4 ! { dg-warning "is an array and OPTIONAL" }
   end subroutine

   elemental function fun2 (arg1,arg2)
      integer,intent(in), optional :: arg1
      integer,intent(in), optional :: arg2
      integer                      :: fun2
      fun2 = arg2
   end function
end subroutine two
