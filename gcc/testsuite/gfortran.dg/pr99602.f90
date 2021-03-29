! { dg-do compile }
! { dg-options "-fcheck=pointer -fdump-tree-original" }
!
! Test fix of PR99602, where a spurious runtime error was introduced
! by PR99112. This is the testcase in comment #6 of the PR.
! PR99602a.f90 turns on the runtime errors by eliminating the pointer
! attribute from the formal arguments in the abstract interface and
! prepare_whizard_m2.
!
! Contributed by Jeurgen Reuter  <juergen.reuter@desy.de>
!
module m
  implicit none
  private
  public :: m_t
  type :: m_t
     private
  end type m_t
end module m

module m2_testbed
  use m
  implicit none
  private
  public :: prepare_m2
  procedure (prepare_m2_proc), pointer :: prepare_m2 => null ()

  abstract interface
     subroutine prepare_m2_proc (m2)
       import
       class(m_t), intent(inout), pointer :: m2
     end subroutine prepare_m2_proc
  end interface

end module m2_testbed

module a
  use m
  use m2_testbed, only: prepare_m2
  implicit none
  private
  public :: a_1

contains

  subroutine a_1 ()
    class(m_t), pointer :: mm
    mm => null ()
    call prepare_m2 (mm) ! Runtime error triggered here
  end subroutine a_1

end module a


module m2
  use m
  implicit none
  private
  public :: m2_t

  type, extends (m_t) :: m2_t
     private
   contains
     procedure :: read => m2_read
  end type m2_t
contains

  subroutine m2_read (mm)
    class(m2_t), intent(out), target :: mm
  end subroutine m2_read
end module m2

program main
  use m2_testbed
  use a, only: a_1
  implicit none
  prepare_m2 => prepare_whizard_m2
  call a_1 ()

contains

  subroutine prepare_whizard_m2 (mm)
    use m
    use m2
    class(m_t), intent(inout), pointer :: mm
    if (.not. associated (mm))  allocate (m2_t :: mm)
    select type (mm)
    type is (m2_t)
!       call mm%read ()  ! Since mm is passed to non-pointer, this generates the error code.
    end select
  end subroutine prepare_whizard_m2
end program main
! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 0 "original" } }
! { dg-final { scan-tree-dump-times "Pointer actual argument" 0 "original" } }
