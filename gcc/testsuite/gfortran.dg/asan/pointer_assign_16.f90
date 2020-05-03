! { dg-do run }
! PR fortran/94788 - this leads to a double free.
! Test case by Juergen Reuter.
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
  private :: op_eq_VS_VS
  private :: op_eq_CH_VS
  private :: op_eq_VS_CH
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

  elemental function op_eq_VS_VS (string_a, string_b) result (op_eq)
    type(varying_string), intent(in) :: string_a
    type(varying_string), intent(in) :: string_b
    logical                          :: op_eq
    op_eq = char(string_a) == char(string_b)
  end function op_eq_VS_VS

  elemental function op_eq_CH_VS (string_a, string_b) result (op_eq)
    character(LEN=*), intent(in)     :: string_a
    type(varying_string), intent(in) :: string_b
    logical                          :: op_eq
    op_eq = string_a == char(string_b)
  end function op_eq_CH_VS

  elemental function op_eq_VS_CH (string_a, string_b) result (op_eq)
    type(varying_string), intent(in) :: string_a
    character(LEN=*), intent(in)     :: string_b
    logical                          :: op_eq
    op_eq = char(string_a) == string_b
  end function op_eq_VS_CH


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


module parser
  implicit none
  private
  public :: parse_node_t
  public :: parse_tree_t
  type :: parse_node_t
     private
  end type parse_node_t

  type :: parse_tree_t
     private
     type(parse_node_t), pointer :: root_node => null ()
   contains
     procedure :: get_root_ptr => parse_tree_get_root_ptr
  end type parse_tree_t

contains
  function parse_tree_get_root_ptr (parse_tree) result (node)
    class(parse_tree_t), intent(in) :: parse_tree
    type(parse_node_t), pointer :: node
    node => parse_tree%root_node
  end function parse_tree_get_root_ptr

end module parser



module rt_data
  use iso_varying_string, string_t => varying_string
  use parser, only: parse_node_t
  implicit none
  private

  public :: rt_data_t

  type :: rt_parse_nodes_t
     type(parse_node_t), pointer :: weight_expr => null ()
  end type rt_parse_nodes_t

  type :: rt_data_t
     type(rt_parse_nodes_t) :: pn
     type(string_t) :: logfile
   contains
     procedure :: global_init => rt_data_global_init
     procedure :: local_init => rt_data_local_init
     procedure :: activate => rt_data_activate
  end type rt_data_t


contains

  subroutine rt_data_global_init (global, logfile)
    class(rt_data_t), intent(out), target :: global
    type(string_t), intent(in), optional :: logfile
    integer :: seed
    if (present (logfile)) then
       global%logfile = logfile
    else
       global%logfile = ""
    end if
    call system_clock (seed)
  end subroutine rt_data_global_init

  subroutine rt_data_local_init (local, global, env)
    class(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(in), target :: global
    integer, intent(in), optional :: env
    local%logfile = global%logfile
  end subroutine rt_data_local_init

  subroutine rt_data_activate (local)
    class(rt_data_t), intent(inout), target :: local
    class(rt_data_t), pointer :: global
    
    ! global => local%context
    ! if (associated (global)) then
    !    local%logfile = global%logfile
    !    local%pn = global%pn
    ! end if
  end subroutine rt_data_activate

end module rt_data

module events
  implicit none
  private
  public :: event_t

  type :: event_config_t
  end type event_config_t

  type :: event_t
     type(event_config_t) :: config
  end type event_t

end module events


module simulations
  use iso_varying_string, string_t => varying_string
  use events 
  use rt_data

  implicit none
  private

  public :: simulation_t

  type, extends (event_t) :: entry_t
     private
     type(entry_t), pointer :: next => null ()
  end type entry_t

  type, extends (entry_t) :: alt_entry_t
   contains
     procedure :: init_alt => alt_entry_init
  end type alt_entry_t

  type :: simulation_t
     private
     type(rt_data_t), pointer :: local => null ()
     integer :: n_alt = 0
     type(entry_t), dimension(:), allocatable :: entry
     type(alt_entry_t), dimension(:,:), allocatable :: alt_entry
   contains
     procedure :: init => simulation_init
  end type simulation_t


contains

  subroutine alt_entry_init (entry, local)
    class(alt_entry_t), intent(inout), target :: entry
    type(rt_data_t), intent(inout), target :: local
    integer :: i
  end subroutine alt_entry_init

  subroutine simulation_init (simulation, &
       integrate, generate, local, global, alt_env)
    class(simulation_t), intent(out), target :: simulation
    logical, intent(in) :: integrate, generate
    type(rt_data_t), intent(inout), target :: local
    type(rt_data_t), intent(inout), optional, target :: global
    type(rt_data_t), dimension(:), intent(inout), optional, target :: alt_env
    simulation%local => local
    allocate (simulation%entry (1))
    if (present (alt_env)) then
       simulation%n_alt = size (alt_env)
    end if
  end subroutine simulation_init

end module simulations


program main_ut
  use iso_varying_string, string_t => varying_string
  use parser, only: parse_tree_t
  use rt_data
  use simulations
  implicit none
  call simulations_10 (6)

contains

  subroutine simulations_10 (u)
    integer, intent(in) :: u
    type(rt_data_t), target :: global
    type(rt_data_t), dimension(1), target :: alt_env
    type(parse_tree_t) :: pt_weight
    type(simulation_t), target :: simulation

    call global%global_init ()
    call alt_env(1)%local_init (global)
    call alt_env(1)%activate ()

    !!!! This causes the pointer hiccup
    alt_env(1)%pn%weight_expr => pt_weight%get_root_ptr ()
    call simulation%init (.true., .true., global, alt_env=alt_env)

  end subroutine simulations_10
  
end program main_ut
