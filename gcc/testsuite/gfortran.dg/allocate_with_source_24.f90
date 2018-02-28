! { dg-do run }
!
! Test that the temporary in a sourced-ALLOCATE is not freeed.
! PR fortran/79344
! Contributed by Juergen Reuter

module iso_varying_string
  implicit none

  type, public :: varying_string
     private
     character(LEN=1), dimension(:), allocatable :: chars
  end type varying_string

  interface assignment(=)
     module procedure op_assign_VS_CH
  end interface assignment(=)

  interface operator(/=)
     module procedure op_not_equal_VS_CA
  end interface operator(/=)

  interface len
     module procedure len_
  end interface len

  interface var_str
     module procedure var_str_
  end interface var_str

  public :: assignment(=)
  public :: operator(/=)
  public :: len

  private :: op_assign_VS_CH
  private :: op_not_equal_VS_CA
  private :: char_auto
  private :: len_
  private :: var_str_

contains

  elemental function len_ (string) result (length)
    type(varying_string), intent(in) :: string
    integer                          :: length
    if(ALLOCATED(string%chars)) then
       length = SIZE(string%chars)
    else
       length = 0
    endif
  end function len_

  elemental subroutine op_assign_VS_CH (var, exp)
    type(varying_string), intent(out) :: var
    character(LEN=*), intent(in)      :: exp
    var = var_str(exp)
  end subroutine op_assign_VS_CH

  pure function op_not_equal_VS_CA (var, exp) result(res)
    type(varying_string), intent(in) :: var
    character(LEN=*), intent(in)     :: exp
    logical :: res
    integer :: i
    res = .true.
    if (len(exp) /= size(var%chars)) return
    do i = 1, size(var%chars)
      if (var%chars(i) /= exp(i:i)) return
    end do
    res = .false.
  end function op_not_equal_VS_CA

  pure function char_auto (string) result (char_string)
    type(varying_string), intent(in) :: string
    character(LEN=len(string))       :: char_string
    integer                          :: i_char
    forall(i_char = 1:len(string))
       char_string(i_char:i_char) = string%chars(i_char)
    end forall
  end function char_auto

  elemental function var_str_ (char) result (string)
    character(LEN=*), intent(in) :: char
    type(varying_string)         :: string
    integer                      :: length
    integer                      :: i_char
    length = LEN(char)
    ALLOCATE(string%chars(length))
    forall(i_char = 1:length)
       string%chars(i_char) = char(i_char:i_char)
    end forall
  end function var_str_

end module iso_varying_string

!!!!!
 
program test_pr79344

  use iso_varying_string, string_t => varying_string

  implicit none

  type :: field_data_t
     type(string_t), dimension(:), allocatable :: name
  end type field_data_t

  type(field_data_t) :: model, model2
  allocate(model%name(2))
  model%name(1) = "foo"
  model%name(2) = "bar"
  call copy(model, model2)
contains

  subroutine copy(prt, prt_src)
    implicit none
    type(field_data_t), intent(inout) :: prt
    type(field_data_t), intent(in) :: prt_src
    integer :: i
    if (allocated (prt_src%name)) then
       if (prt_src%name(1) /= "foo") STOP 1
       if (prt_src%name(2) /= "bar") STOP 2

       if (allocated (prt%name))  deallocate (prt%name)
       allocate (prt%name (size (prt_src%name)), source = prt_src%name)
       ! The issue was, that prt_src was empty after sourced-allocate.
       if (prt_src%name(1) /= "foo") STOP 3
       if (prt_src%name(2) /= "bar") STOP 4
       if (prt%name(1) /= "foo") STOP 5
       if (prt%name(2) /= "bar") STOP 6
    end if
  end subroutine copy

end program test_pr79344

