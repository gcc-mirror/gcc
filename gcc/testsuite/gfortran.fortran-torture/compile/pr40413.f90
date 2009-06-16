module state_matrices

  implicit none
  private

  public :: state_matrix_copy
  public :: state_matrix_t
  public :: matrix_element_t

  type :: matrix_element_t
     private
     integer, dimension(:), allocatable :: f
  end type matrix_element_t

  type :: state_matrix_t
     private
     type(matrix_element_t), dimension(:), allocatable :: me
  end type state_matrix_t

  type :: polarization_t
     logical :: polarized = .false.
     integer :: spin_type = 0
     integer :: multiplicity = 0
     type(state_matrix_t) :: state
  end type polarization_t

contains

  function polarization_copy (pol_in) result (pol)
    type(polarization_t) :: pol
    type(polarization_t), intent(in) :: pol_in
    !!! type(state_matrix_t) :: state_dummy
    pol%polarized = pol_in%polarized
    pol%spin_type = pol_in%spin_type
    pol%multiplicity = pol_in%multiplicity
    !!! state_dummy = state_matrix_copy (pol_in%state)
    !!! pol%state = state_dummy
    pol%state = state_matrix_copy (pol_in%state)
  end function polarization_copy

  function state_matrix_copy (state_in) result (state)
    type(state_matrix_t) :: state
    type(state_matrix_t), intent(in), target :: state_in
  end function state_matrix_copy

end module state_matrices
