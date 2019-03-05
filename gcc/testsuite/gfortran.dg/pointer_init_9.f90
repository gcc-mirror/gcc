! { dg-do compile }
! PR 71237 - this used to ICE.
module data_mod
  implicit none

  type data_t
    integer :: i
  end type

  type(data_t), pointer :: data
  integer, pointer :: idata => data%i ! { dg-error "Pointer assignment target in initialization expression does not have the TARGET attribute" }

end module
