! { dg-do compile }
!
! PR 47463: [OOP] ICE in gfc_add_component_ref
!
! Contributed by Rich Townsend <townsend@astro.wisc.edu>

module hydro_state
  type :: state_t
   contains
     procedure :: assign
     generic   :: assignment(=) => assign
  end type state_t
contains
  subroutine assign (this, that)
    class(state_t), intent(inout) :: this
    class(state_t), intent(in)    :: that
  end subroutine assign
end module hydro_state

module hydro_flow
  use hydro_state
  type :: flow_t
     class(state_t), allocatable :: st
  end type flow_t
contains
  subroutine init_comps (this, st)
    class(flow_t), intent(out) :: this
    class(state_t), intent(in) :: st

    allocate(state_t :: this%st)
    this%st = st
  end subroutine init_comps
end module hydro_flow 

! { dg-final { cleanup-modules "hydro_state hydro_flow" } }
