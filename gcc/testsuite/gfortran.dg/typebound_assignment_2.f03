! { dg-do compile }
!
! PR 47463: [OOP] ICE in gfc_add_component_ref
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

module hydro_grid
  type :: grid_t
   contains
     procedure :: assign
     generic   :: assignment(=) => assign
  end type grid_t
  public :: grid_t
contains
  subroutine assign (this, that)
    class(grid_t), intent(inout) :: this
    class(grid_t), intent(in)    :: that
  end subroutine assign
end module hydro_grid

module hydro_flow
  use hydro_grid
  type :: flow_t
     class(grid_t), allocatable  :: gr
  end type flow_t
contains
  subroutine init_params (this)
    class(flow_t), intent(out) :: this
    type(grid_t)               :: gr
   call init_comps(this, gr)
  end subroutine init_params
  subroutine init_comps (this, gr)
    class(flow_t), intent(out) :: this
    class(grid_t), intent(in)  :: gr
    this%gr = gr
  end subroutine init_comps
end module hydro_flow 
