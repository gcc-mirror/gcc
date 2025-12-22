! { dg-do compile }
! PR fortran/123253 - pointer assignment checks in SELECT TYPE
!
! Contributed by Jürgen Reuter

module vamp
  implicit none
  private
  type, public :: vamp_data_t
  end type vamp_data_t
end module vamp

module mci_vamp
  use vamp !NODEP!
  implicit none
  private

  type, abstract :: mci_sampler_t
  end type mci_sampler_t

  type :: mci_vamp_t
   contains
     procedure :: generate_weighted_event => mci_vamp_generate_weighted_event
  end type mci_vamp_t

  type, extends (vamp_data_t) :: mci_workspace_t
     class(mci_sampler_t), pointer :: sampler => null ()
     class(mci_vamp_instance_t), pointer :: instance => null ()
  end type mci_workspace_t

  type :: mci_vamp_instance_t
     type(mci_vamp_t), pointer :: mci => null ()
  end type mci_vamp_instance_t

contains

  subroutine mci_vamp_generate_weighted_event (mci, instance, sampler)
    class(mci_vamp_t), intent(inout) :: mci
    class(mci_vamp_instance_t), intent(inout), target :: instance
    class(mci_sampler_t), intent(inout), target :: sampler
    class(vamp_data_t), allocatable :: data

    select type (instance)
    type is (mci_vamp_instance_t)
       allocate (mci_workspace_t :: data)
       select type (data)
       type is (mci_workspace_t)
          data%sampler => sampler
          data%instance => instance
       end select
    end select

    select type (foo_instance => instance)
    type is (mci_vamp_instance_t)
       allocate (mci_workspace_t :: data)
       select type (tmp => data)
       type is (mci_workspace_t)
          tmp%sampler => sampler
          tmp%instance => foo_instance
       end select
    end select

  end subroutine mci_vamp_generate_weighted_event

end module mci_vamp
