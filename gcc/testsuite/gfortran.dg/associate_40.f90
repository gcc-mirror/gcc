! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for the second part of PR87359 in which the reallocation on
! assignment for components of associate names was disallowed by r264358.
! -fcheck-all exposed the mismatch in array shapes. The deallocations at
! the end of the main program are there to make sure that valgrind does
! not report an memory leaks.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module phs_fks
  implicit none
  private
  public :: phs_identifier_t
  public :: phs_fks_t
  type :: phs_identifier_t
     integer, dimension(:), allocatable :: contributors
  contains
    procedure :: init => phs_identifier_init
  end type phs_identifier_t

  type :: phs_fks_t
     type(phs_identifier_t), dimension(:), allocatable :: phs_identifiers
  end type phs_fks_t
contains

  subroutine phs_identifier_init &
     (phs_id, contributors)
     class(phs_identifier_t), intent(out) :: phs_id
     integer, intent(in), dimension(:) :: contributors
     allocate (phs_id%contributors (size (contributors)))
     phs_id%contributors = contributors
   end subroutine phs_identifier_init

end module phs_fks

!!!!!

module instances
  use phs_fks
  implicit none
  private
  public :: process_instance_t

  type :: nlo_event_deps_t
     type(phs_identifier_t), dimension(:), allocatable :: phs_identifiers
  end type nlo_event_deps_t

  type :: process_instance_t
     type(phs_fks_t), pointer :: phs => null ()
     type(nlo_event_deps_t) :: event_deps
   contains
     procedure :: init => process_instance_init
     procedure :: setup_real_event_kinematics => pi_setup_real_event_kinematics
  end type process_instance_t

contains

  subroutine process_instance_init (instance)
    class(process_instance_t), intent(out), target :: instance
    integer :: i
    integer :: i_born, i_real
    allocate (instance%phs)
  end subroutine process_instance_init

  subroutine pi_setup_real_event_kinematics (process_instance)
    class(process_instance_t), intent(inout) :: process_instance
    integer :: i_real, i
    associate (event_deps => process_instance%event_deps)
       i_real = 2
       associate (phs => process_instance%phs)
          allocate (phs%phs_identifiers (3))
          call phs%phs_identifiers(1)%init ([1])
          call phs%phs_identifiers(2)%init ([1,2])
          call phs%phs_identifiers(3)%init ([1,2,3])
          process_instance%event_deps%phs_identifiers = phs%phs_identifiers  ! Error: mismatch in array shapes.
       end associate
    end associate
  end subroutine pi_setup_real_event_kinematics

end module instances

!!!!!

program main
  use instances, only: process_instance_t
  implicit none
  type(process_instance_t), allocatable, target :: process_instance
  allocate (process_instance)
  call process_instance%init ()
  call process_instance%setup_real_event_kinematics ()
  if (associated (process_instance%phs)) deallocate (process_instance%phs)
  if (allocated (process_instance)) deallocate (process_instance)
end program main
! { dg-final { scan-tree-dump-times "__builtin_realloc" 2 "original" } }
