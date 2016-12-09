! { dg-do run }
!
! PR 61767: [OOP] ICE in generate_finalization_wrapper at fortran/class.c:1491
!
! Contributed by <reubendb@gmail.com>

module Communicator_Form
  implicit none
  type :: CommunicatorForm
  contains
    final :: Finalize
  end type
  type :: MessageTemplate
    type ( CommunicatorForm ), pointer :: Communicator
  end type
contains
  subroutine Finalize ( C )
    type ( CommunicatorForm ) :: C
    ! should not be called
    call abort()
  end subroutine
end module

program p
  use Communicator_Form
  implicit none
  class ( MessageTemplate ), pointer :: M
  allocate(M)
  deallocate(M)
end
