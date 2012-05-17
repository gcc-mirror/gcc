module bind_c_dts_2
use, intrinsic :: iso_c_binding
implicit none
type, bind(c) :: my_c_type_1
   integer(c_int) :: j
end type my_c_type_1
contains
  subroutine sub0(my_type, expected_j) bind(c)
    type(my_c_type_1) :: my_type
    integer(c_int), value :: expected_j
    if (my_type%j .ne. expected_j) then
       call abort ()
    end if
  end subroutine sub0
end module bind_c_dts_2
