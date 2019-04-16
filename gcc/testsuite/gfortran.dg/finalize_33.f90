! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Tests the fix for PR87359 in which the finalization of
! 'source=process%component%extract_mci_template()' in the allocation
! of 'process%mci' caused invalid reads and freeing of already freed
! memory. This test is a greatly reduced version of the original code.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module mci_base
  implicit none
  private
  public :: mci_t
  public :: mci_midpoint_t
  public :: cnt
  integer :: cnt = 0
  type, abstract :: mci_t
     integer, dimension(:), allocatable :: chain
  end type mci_t
  type, extends (mci_t) :: mci_midpoint_t
  contains
    final :: mci_midpoint_final
  end type mci_midpoint_t
contains
  IMPURE ELEMENTAL SUBROUTINE mci_midpoint_final(arg)
    TYPE(mci_midpoint_t), INTENT(INOUT) :: arg
    cnt = cnt + 1
  END SUBROUTINE mci_midpoint_final
end module mci_base

!!!!!

module process_config
  use mci_base
  implicit none
  private
  public :: process_component_t
  type :: process_component_t
     class(mci_t), allocatable :: mci_template
   contains
     procedure :: init => process_component_init
     procedure :: extract_mci_template => process_component_extract_mci_template
  end type process_component_t

contains

  subroutine process_component_init (component, mci_template)
    class(process_component_t), intent(out) :: component
    class(mci_t), intent(in), allocatable :: mci_template
    if (allocated (mci_template)) &
         allocate (component%mci_template, source = mci_template)
  end subroutine process_component_init

  function process_component_extract_mci_template (component) &
         result (mci_template)
    class(mci_t), allocatable :: mci_template
    class(process_component_t), intent(in) :: component
    if (allocated (component%mci_template)) &
       allocate (mci_template, source = component%mci_template)
  end function process_component_extract_mci_template
end module process_config

!!!!!

module process
  use mci_base
  use process_config
  implicit none
  private
  public :: process_t
  type :: process_t
     private
     type(process_component_t) :: component
     class(mci_t), allocatable :: mci
   contains
     procedure :: init_component => process_init_component
     procedure :: setup_mci => process_setup_mci
  end type process_t
contains
  subroutine process_init_component &
       (process, mci_template)
    class(process_t), intent(inout), target :: process
    class(mci_t), intent(in), allocatable :: mci_template
    call process%component%init (mci_template)
  end subroutine process_init_component

  subroutine process_setup_mci (process)
    class(process_t), intent(inout) :: process
    allocate (process%mci, source=process%component%extract_mci_template ())
  end subroutine process_setup_mci

end module process

!!!!!

program main_ut
  use mci_base
  use process, only: process_t
  implicit none
  call event_transforms_1 ()
  if (cnt .ne. 4) stop 2
contains

  subroutine event_transforms_1 ()
    class(mci_t), allocatable :: mci_template
    type(process_t), allocatable, target :: process
    allocate (process)
    allocate (mci_midpoint_t :: mci_template)
    call process%init_component (mci_template)
    call process%setup_mci ()                  ! generates 1 final call from call to extract_mci_template
    if (cnt .ne. 1) stop 1
  end subroutine event_transforms_1            ! generates 3 final calls to mci_midpoint_final:
                                               ! (i) process%component%mci_template
                                               ! (ii) process%mci
                                               ! (iii) mci_template
end program main_ut
! { dg-final { scan-tree-dump-times "__builtin_malloc" 17 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 19 "original" } }
