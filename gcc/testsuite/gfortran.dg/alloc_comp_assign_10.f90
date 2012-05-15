! { dg-do run }
!
! Test the fix for PR39879, in which gfc gagged on the double
! defined assignment where the rhs had a default initialiser.
!
! Contributed by David Sagan <david.sagan@gmail.com>
!
module test_struct
  interface assignment (=)
    module procedure tao_lat_equal_tao_lat
  end interface
  type bunch_params_struct
    integer n_live_particle          
  end type
  type tao_lattice_struct
    type (bunch_params_struct), allocatable :: bunch_params(:)
    type (bunch_params_struct), allocatable :: bunch_params2(:)
  end type
  type tao_universe_struct
    type (tao_lattice_struct), pointer :: model, design
    character(200), pointer :: descrip => NULL()
  end type
  type tao_super_universe_struct
    type (tao_universe_struct), allocatable :: u(:)          
  end type
  type (tao_super_universe_struct), save, target :: s
  contains
    subroutine tao_lat_equal_tao_lat (lat1, lat2)
      implicit none
      type (tao_lattice_struct), intent(inout) :: lat1
      type (tao_lattice_struct), intent(in) :: lat2
      if (allocated(lat2%bunch_params)) then
        lat1%bunch_params = lat2%bunch_params
      end if 
      if (allocated(lat2%bunch_params2)) then
        lat1%bunch_params2 = lat2%bunch_params2
      end if 
    end subroutine
end module

program tao_program
  use test_struct
  implicit none
  type (tao_universe_struct), pointer :: u
  integer n, i
  allocate (s%u(1))
  u => s%u(1)
  allocate (u%design, u%model)
  n = 112
  allocate (u%model%bunch_params(0:n), u%design%bunch_params(0:n))
  u%design%bunch_params%n_live_particle = [(i, i = 0, n)]
  u%model = u%design
  u%model = u%design ! The double assignment was the cause of the ICE
  if (.not. allocated (u%model%bunch_params)) call abort
  if (any (u%model%bunch_params%n_live_particle .ne. [(i, i = 0, n)])) call abort
  Deallocate (u%model%bunch_params, u%design%bunch_params)
  deallocate (u%design, u%model)
  deallocate (s%u)
end program
