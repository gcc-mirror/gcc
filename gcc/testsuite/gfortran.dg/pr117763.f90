! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR117763, which was a regression caused by the patch for
! PR109345.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module iso_varying_string
  implicit none
  integer, parameter, private :: GET_BUFFER_LEN = 1

  type, public :: varying_string
     private
     character(LEN=1), dimension(:), allocatable :: chars
  end type varying_string

  interface assignment(=)
     module procedure op_assign_CH_VS
     module procedure op_assign_VS_CH
  end interface assignment(=)

  interface char
     module procedure char_auto
     module procedure char_fixed
  end interface char

  interface len
     module procedure len_
  end interface len

  interface var_str
     module procedure var_str_
  end interface var_str

  public :: assignment(=)
  public :: char
  public :: len
  public :: var_str

  private :: op_assign_CH_VS
  private :: op_assign_VS_CH
  private :: char_auto
  private :: char_fixed
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

  elemental subroutine op_assign_CH_VS (var, exp)
    character(LEN=*), intent(out)    :: var
    type(varying_string), intent(in) :: exp
    var = char(exp)
  end subroutine op_assign_CH_VS

  elemental subroutine op_assign_VS_CH (var, exp)
    type(varying_string), intent(out) :: var
    character(LEN=*), intent(in)      :: exp
    var = var_str(exp)
  end subroutine op_assign_VS_CH

  pure function char_auto (string) result (char_string)
    type(varying_string), intent(in) :: string
    character(LEN=len(string))       :: char_string
    integer                          :: i_char
    forall(i_char = 1:len(string))
       char_string(i_char:i_char) = string%chars(i_char)
    end forall
  end function char_auto

  pure function char_fixed (string, length) result (char_string)
    type(varying_string), intent(in) :: string
    integer, intent(in)              :: length
    character(LEN=length)            :: char_string
    char_string = char(string)
  end function char_fixed

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

module model_data
  use, intrinsic :: iso_c_binding !NODEP!
  use iso_varying_string, string_t => varying_string

  implicit none
  private

  public :: field_data_t
  public :: model_data_t

  type :: field_data_t
     private
     type(string_t) :: longname
     integer :: pdg = 0
     logical :: has_anti = .false.
     type(string_t), dimension(:), allocatable :: name, anti
     type(string_t) :: tex_name
     integer :: multiplicity = 1
   contains
     procedure :: init => field_data_init
     procedure :: set => field_data_set
     procedure :: get_longname => field_data_get_longname
     procedure :: get_name_array => field_data_get_name_array
  end type field_data_t

  type :: model_data_t
     private
     type(field_data_t), dimension(:), allocatable :: field
   contains
     generic :: init => model_data_init
     procedure, private :: model_data_init
     procedure :: get_field_array_ptr => model_data_get_field_array_ptr
     procedure :: get_field_ptr_by_index => model_data_get_field_ptr_index
     procedure :: init_sm_test => model_data_init_sm_test
  end type model_data_t


contains

  subroutine field_data_init (prt, longname, pdg)
    class(field_data_t), intent(out) :: prt
    type(string_t), intent(in) :: longname
    integer, intent(in) :: pdg
    prt%longname = longname
    prt%pdg = pdg
    prt%tex_name = ""
  end subroutine field_data_init

  subroutine field_data_set (prt, &
       name, anti, tex_name)
    class(field_data_t), intent(inout) :: prt
    type(string_t), dimension(:), intent(in), optional :: name, anti
    type(string_t), intent(in), optional :: tex_name
    if (present (name)) then
       if (allocated (prt%name))  deallocate (prt%name)
       allocate (prt%name (size (name)), source = name)
    end if
    if (present (anti)) then
       if (allocated (prt%anti))  deallocate (prt%anti)
       allocate (prt%anti (size (anti)), source = anti)
       prt%has_anti = .true.
    end if
    if (present (tex_name))  prt%tex_name = tex_name
  end subroutine field_data_set

  pure function field_data_get_longname (prt) result (name)
    type(string_t) :: name
    class(field_data_t), intent(in) :: prt
    name = prt%longname
  end function field_data_get_longname

  subroutine field_data_get_name_array (prt, is_antiparticle, name)
    class(field_data_t), intent(in) :: prt
    logical, intent(in) :: is_antiparticle
    type(string_t), dimension(:), allocatable, intent(inout) :: name
    if (allocated (name))  deallocate (name)
    if (is_antiparticle) then
       if (prt%has_anti) then
          allocate (name (size (prt%anti)))
          name = prt%anti
       else
          allocate (name (0))
       end if
    else
       allocate (name (size (prt%name)))
       name = prt%name
    end if
  end subroutine field_data_get_name_array

  subroutine model_data_init (model, n_field)
    class(model_data_t), intent(out) :: model
    integer, intent(in) :: n_field
    allocate (model%field (n_field))
  end subroutine model_data_init

  function model_data_get_field_array_ptr (model) result (ptr)
    class(model_data_t), intent(in), target :: model
    type(field_data_t), dimension(:), pointer :: ptr
    ptr => model%field
  end function model_data_get_field_array_ptr

  function model_data_get_field_ptr_index (model, i) result (ptr)
    class(model_data_t), intent(in), target :: model
    integer, intent(in) :: i
    type(field_data_t), pointer :: ptr
    ptr => model%field(i)
  end function model_data_get_field_ptr_index

  subroutine model_data_init_sm_test (model)
    class(model_data_t), intent(out) :: model
    type(field_data_t), pointer :: field
    integer :: i
    call model%init (2)
    i = 0
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("W_BOSON"), 24)
    call field%set (name = [var_str ("W+")], anti = [var_str ("W-")])
    i = i + 1
    field => model%get_field_ptr_by_index (i)
    call field%init (var_str ("HIGGS"), 25)
    call field%set (name = [var_str ("H")])
  end subroutine model_data_init_sm_test

end module model_data


module models
  use, intrinsic :: iso_c_binding !NODEP!
  use iso_varying_string, string_t => varying_string
  use model_data
!  use parser
!  use variables
  implicit none
  private
  public :: model_t

  type, extends (model_data_t) :: model_t
     private
   contains
     procedure :: append_field_vars => model_append_field_vars
  end type model_t

contains

  subroutine model_append_field_vars (model)
    class(model_t), intent(inout) :: model
    type(field_data_t), dimension(:), pointer :: field_array
    type(field_data_t), pointer :: field
    type(string_t) :: name
    type(string_t), dimension(:), allocatable :: name_array
    integer :: i, j
    field_array => model%get_field_array_ptr ()
    do i = 1, size (field_array)
       name = field_array(i)%get_longname ()
       call field_array(i)%get_name_array (.false., name_array)
    end do
  end subroutine model_append_field_vars

end module models


program main_ut
  use iso_varying_string, string_t => varying_string
  use model_data
  use models
  implicit none

  class(model_data_t), pointer :: model
  model => null ()
  allocate (model_t :: model)
  select type (model)
  type is (model_t)
     call model%init_sm_test ()
     call model%append_field_vars ()
  end select
end program main_ut
! { dg-final { scan-tree-dump-times "__result->span = \[12\].." 1 "original" } }
