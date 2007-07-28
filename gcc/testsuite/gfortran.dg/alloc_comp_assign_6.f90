! { dg-do run }
! Tests the fix for pr32880, in which 'res' was deallocated
! before it could be used in the concatenation.
! Adapted from vst28.f95, in Lawrie Schonfeld's iso_varying_string
! testsuite, by Tobias Burnus.
!
module iso_varying_string
  type varying_string
     character(LEN=1), dimension(:), allocatable :: chars
  end type varying_string
  interface assignment(=)
     module procedure op_assign_VS_CH
  end interface assignment(=)
  interface operator(//)
     module procedure op_concat_VS_CH
  end interface operator(//)
contains
  elemental subroutine op_assign_VS_CH (var, exp)
    type(varying_string), intent(out) :: var
    character(LEN=*), intent(in)      :: exp
    integer                      :: length
    integer                      :: i_char
    length = len(exp)
    allocate(var%chars(length))
    forall(i_char = 1:length)
       var%chars(i_char) = exp(i_char:i_char)
    end forall
  end subroutine op_assign_VS_CH
  elemental function op_concat_VS_CH (string_a, string_b) result (concat_string)
    type(varying_string), intent(in) :: string_a
    character(LEN=*), intent(in)     :: string_b
    type(varying_string)             :: concat_string
    len_string_a = size(string_a%chars)
    allocate(concat_string%chars(len_string_a+len(string_b)))
    if (len_string_a >0) &
       concat_string%chars(:len_string_a) = string_a%chars
    if (len (string_b) > 0) &
       concat_string%chars(len_string_a+1:) = string_b
  end function op_concat_VS_CH
end module iso_varying_string

program VST28
  use iso_varying_string
  character(len=10) :: char_a
  type(VARYING_STRING) :: res
  char_a = "abcdefghij"
  res = char_a(5:5)
  res = res//char_a(6:6)
  if(size(res%chars) /= 2 .or. any(res%chars /= ['e','f'])) then
    write(*,*) 'ERROR: should be ef, got: ', res%chars, size(res%chars)
    call abort ()
  end if
end program VST28

! { dg-final { cleanup-modules "iso_varying_string" } }
