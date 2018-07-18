! { dg-do run }
! { dg-additional-sources bind_c_dts_2_driver.c }
module bind_c_dts_2
use, intrinsic :: iso_c_binding
implicit none

type, bind(c) :: my_c_type_0
   integer(c_int) :: i
   type(c_ptr) :: nested_c_address
   integer(c_int) :: array(3)
end type my_c_type_0

type, bind(c) :: my_c_type_1
   type(my_c_type_0) :: my_nested_type
   type(c_ptr) :: c_address
   integer(c_int) :: j
end type my_c_type_1

contains
  subroutine sub0(my_type, expected_i, expected_nested_c_address, &
       expected_array_1, expected_array_2, expected_array_3, &
       expected_c_address, expected_j) bind(c)
    type(my_c_type_1) :: my_type
    integer(c_int), value :: expected_i
    type(c_ptr), value :: expected_nested_c_address
    integer(c_int), value :: expected_array_1
    integer(c_int), value :: expected_array_2
    integer(c_int), value :: expected_array_3
    type(c_ptr), value :: expected_c_address
    integer(c_int), value :: expected_j

    if (my_type%my_nested_type%i .ne. expected_i) then
       STOP 1
    end if

    if (.not. c_associated(my_type%my_nested_type%nested_c_address, &
         expected_nested_c_address)) then
       STOP 2
    end if

    if (my_type%my_nested_type%array(1) .ne. expected_array_1) then
       STOP 3
    end if

    if (my_type%my_nested_type%array(2) .ne. expected_array_2) then
       STOP 4
    end if

    if (my_type%my_nested_type%array(3) .ne. expected_array_3) then
       STOP 5
    end if

    if (.not. c_associated(my_type%c_address, expected_c_address)) then
       STOP 6
    end if

    if (my_type%j .ne. expected_j) then
       STOP 7
    end if
  end subroutine sub0
end module bind_c_dts_2
