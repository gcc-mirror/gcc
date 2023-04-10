! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test conformance with clause 7.5.6.3, paragraph 6 of F2018. Part of PR106576.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module finalizable_m
  !! This module supports the main program at the bottom of this file, which
  !! tests compiler conformance with clause 7.5.6.3, paragraph 6 in the Fortran
  !! Interpretation Document (https://j3-fortran.org/doc/year/18/18-007r1.pdf):
  !! "If a specification expression in a scoping unit references
  !! a function, the result is finalized before execution of the executable
  !! constructs in the scoping unit."
  implicit none

  private
  public :: finalizable_t, component

  type finalizable_t
    private
    integer, allocatable :: component_
  contains
    final :: finalize
  end Type

  interface finalizable_t
    module procedure construct
  end interface

contains

  pure function construct(component) result(finalizable)
    integer, intent(in) :: component
    type(finalizable_t) finalizable
    allocate(finalizable%component_, source = component)
  end function

  pure function component(self) result(self_component)
    type(finalizable_t), intent(in) :: self
    integer self_component
    self_component = self%component_
  end function

  pure subroutine finalize(self)
    type(finalizable_t), intent(inout) :: self
    if (allocated(self%component_)) deallocate(self%component_)
  end subroutine

end module

program specification_expression_finalization
  use finalizable_m, only : finalizable_t, component
  implicit none

  call finalize_specification_expression_result

contains

  subroutine finalize_specification_expression_result
    real tmp(component(finalizable_t(component=1))) !! Finalizes the finalizable_t function result
    real eliminate_unused_variable_warning
    tmp = eliminate_unused_variable_warning
  end subroutine

end program
! { dg-final { scan-tree-dump-times "_final != 0B" 1 "original" } }