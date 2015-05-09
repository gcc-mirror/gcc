! { dg-do run }
!
! Check error of pr65894 are fixed.
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!                Andre Vehreschild  <vehre@gcc.gnu.org>

module simple_string
  ! Minimal iso_varying_string implementation needed.
  implicit none

  type string_t
    private
    character(len=1), dimension(:), allocatable :: cs
  end type string_t

contains
  elemental function var_str(c) result (s)
    character(*), intent(in) :: c
    type(string_t) :: s
    integer :: l,i

    l = len(c)
    allocate(s%cs(l))
    forall(i = 1:l)
      s%cs(i) = c(i:i)
    end forall
  end function var_str

end module simple_string
module model_data
  use simple_string

  implicit none
  private

  public :: field_data_t
  public :: model_data_t

  type :: field_data_t
     !private
     integer :: pdg = 0
     type(string_t), dimension(:), allocatable :: name
   contains
     procedure :: init => field_data_init
     procedure :: get_pdg => field_data_get_pdg
  end type field_data_t

  type :: model_data_t
     !private
     type(string_t) :: name
     type(field_data_t), dimension(:), allocatable :: field
   contains
     generic :: init => model_data_init
     procedure, private :: model_data_init
     generic :: get_pdg => &
          model_data_get_field_pdg_index
     procedure, private :: model_data_get_field_pdg_index
     generic :: get_field_ptr => &
          model_data_get_field_ptr_pdg
     procedure, private :: model_data_get_field_ptr_pdg
     procedure :: get_field_ptr_by_index => model_data_get_field_ptr_index
     procedure :: init_sm_test => model_data_init_sm_test
  end type model_data_t

contains

  subroutine field_data_init (prt, pdg)
    class(field_data_t), intent(out) :: prt
    integer, intent(in) :: pdg
    prt%pdg = pdg
  end subroutine field_data_init

  elemental function field_data_get_pdg (prt) result (pdg)
    integer :: pdg
    class(field_data_t), intent(in) :: prt
    pdg = prt%pdg
  end function field_data_get_pdg

  subroutine model_data_init (model, name, &
       n_field)
    class(model_data_t), intent(out) :: model
    type(string_t), intent(in) :: name
    integer, intent(in) :: n_field
    model%name = name
    allocate (model%field (n_field))
  end subroutine model_data_init

  function model_data_get_field_pdg_index (model, i) result (pdg)
    class(model_data_t), intent(in) :: model
    integer, intent(in) :: i
    integer :: pdg
    pdg = model%field(i)%get_pdg ()
  end function model_data_get_field_pdg_index

  function model_data_get_field_ptr_pdg (model, pdg, check) result (ptr)
    class(model_data_t), intent(in), target :: model
    integer, intent(in) :: pdg
    logical, intent(in), optional :: check
    type(field_data_t), pointer :: ptr
    integer :: i, pdg_abs
    if (pdg == 0) then
       ptr => null ()
       return
    end if
    pdg_abs = abs (pdg)
    if (lbound(model%field, 1) /= 1) call abort()
    if (ubound(model%field, 1) /= 19) call abort()
    do i = 1, size (model%field)
       if (model%field(i)%get_pdg () == pdg_abs) then
          ptr => model%field(i)
          return
       end if
    end do
    ptr => null ()
  end function model_data_get_field_ptr_pdg

  function model_data_get_field_ptr_index (model, i) result (ptr)
    class(model_data_t), intent(in), target :: model
    integer, intent(in) :: i
    type(field_data_t), pointer :: ptr
    if (lbound(model%field, 1) /= 1) call abort()
    if (ubound(model%field, 1) /= 19) call abort()
    ptr => model%field(i)
  end function model_data_get_field_ptr_index

  subroutine model_data_init_sm_test (model)
    class(model_data_t), intent(out) :: model
    type(field_data_t), pointer :: field
    integer, parameter :: n_field = 19
    call model%init (var_str ("SM_test"), &
         n_field)
    field => model%get_field_ptr_by_index (1)
    call field%init (1)
  end subroutine model_data_init_sm_test

end module model_data

module flavors
  use model_data

  implicit none
  private

  public :: flavor_t

  type :: flavor_t
     private
     integer :: f = 0
     type(field_data_t), pointer :: field_data => null ()
   contains
     generic :: init => &
          flavor_init0_model
     procedure, private :: flavor_init0_model
  end type flavor_t

contains

  impure elemental subroutine flavor_init0_model (flv, f, model)
    class(flavor_t), intent(inout) :: flv
    integer, intent(in) :: f
    class(model_data_t), intent(in), target :: model
    ! Check the field l/ubound at various stages, because w/o the patch
    ! the bounds get mixed up.
    if (lbound(model%field, 1) /= 1) call abort()
    if (ubound(model%field, 1) /= 19) call abort()
    flv%f = f
    flv%field_data => model%get_field_ptr (f, check=.true.)
  end subroutine flavor_init0_model
end module flavors

module beams
  use model_data
  use flavors
  implicit none
  private
  public :: beam_1
  public :: beam_2
contains
  subroutine beam_1 (u)
    integer, intent(in) :: u
    type(flavor_t), dimension(2) :: flv
    real, dimension(2) :: pol_f
    type(model_data_t), target :: model
    call model%init_sm_test ()
    call flv%init ([1,-1], model)
    pol_f(1) = 0.5
  end subroutine beam_1
  subroutine beam_2 (u, model)
    integer, intent(in) :: u
    type(flavor_t), dimension(2) :: flv
    real, dimension(2) :: pol_f
    class(model_data_t), intent(in), target :: model
    call flv%init ([1,-1], model)
    pol_f(1) = 0.5
  end subroutine beam_2
end module beams

module evaluators
  ! This module is just here for a compile check.
  implicit none
  private
  type :: quantum_numbers_mask_t
   contains
     generic :: operator(.or.) => quantum_numbers_mask_or
     procedure, private :: quantum_numbers_mask_or
  end type quantum_numbers_mask_t

  type :: index_map_t
     integer, dimension(:), allocatable :: entry
  end type index_map_t
  type :: prt_mask_t
     logical, dimension(:), allocatable :: entry
  end type prt_mask_t
  type :: qn_mask_array_t
     type(quantum_numbers_mask_t), dimension(:), allocatable :: mask
  end type qn_mask_array_t

contains
  elemental function quantum_numbers_mask_or (mask1, mask2) result (mask)
    type(quantum_numbers_mask_t) :: mask
    class(quantum_numbers_mask_t), intent(in) :: mask1, mask2
  end function quantum_numbers_mask_or

  subroutine make_product_interaction &
      (prt_is_connected, qn_mask_in, qn_mask_rest)
    type(prt_mask_t), dimension(2), intent(in) :: prt_is_connected
    type(qn_mask_array_t), dimension(2), intent(in) :: qn_mask_in
    type(quantum_numbers_mask_t), intent(in) :: qn_mask_rest
    type(index_map_t), dimension(2) :: prt_index_in
    integer :: i
    type(quantum_numbers_mask_t), dimension(:), allocatable :: qn_mask
    allocate (qn_mask (2))
    do i = 1, 2
       qn_mask(prt_index_in(i)%entry) = &
            pack (qn_mask_in(i)%mask, prt_is_connected(i)%entry) &
            .or. qn_mask_rest
      ! Without the patch above line produced an ICE.
    end do
  end subroutine make_product_interaction
end module evaluators
program main
  use beams
  use model_data
  type(model_data_t) :: model
  call model%init_sm_test()
  call beam_1 (6)
  call beam_2 (6, model)
end program main
