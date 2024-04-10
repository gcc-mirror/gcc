! { dg-do run }
! PR fortran/114012
!
! Polymorphic functions were evaluated twice in assignment

program test
  implicit none

  type :: custom_int
     integer :: val = 2
  end type

  interface assignment(=)
     procedure assign
  end interface
  interface operator(-)
     procedure neg
  end interface

  type(custom_int) :: i
  integer          :: count_assign, count_neg

  count_assign = 0
  count_neg    = 0

  i = 1
  if (count_assign /= 1 .or. count_neg /= 0) stop 1

  i = -i
  if (count_assign /= 2 .or. count_neg /= 1) stop 2
  if (i% val /= -1) stop 3

  i = neg(i)
  if (count_assign /= 3 .or. count_neg /= 2) stop 4
  if (i% val /=  1) stop 5

  i = (neg(i))
  if (count_assign /= 4 .or. count_neg /= 3) stop 6
  if (i% val /= -1) stop 7

  i = - neg(i)
  if (count_assign /= 5 .or. count_neg /= 5) stop 8
  if (i% val /= -1) stop 9

contains

  subroutine assign (field, val)
    type(custom_int), intent(out) :: field
    class(*), intent(in) :: val

    count_assign = count_assign + 1

    select type (val)
    type is (integer)
!      print *, " in assign(integer)", field%val, val
       field%val = val
    type is (custom_int)
!      print *, " in assign(custom)", field%val, val%val
       field%val = val%val
    class default
       error stop
    end select

  end subroutine assign

  function neg (input_field) result(output_field)
    type(custom_int), intent(in), target :: input_field
    class(custom_int), allocatable :: output_field
    allocate (custom_int :: output_field)

    count_neg = count_neg + 1

    select type (output_field)
    type is (custom_int)
!      print *, " in neg", output_field%val, input_field%val
       output_field%val = -input_field%val
    class default
       error stop
    end select
  end function neg
end program test
