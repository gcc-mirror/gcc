! { dg-do compile }
!
! Tests the fix for PRs 78013 and 61420, both of which gave a
! no IMPLICIT type message for the procedure pointer at assignment.
!
module m

  implicit none

  abstract interface
    function I_f() result( r )
      real :: r
    end function I_f
  end interface

  type, abstract :: a_t
    private
    procedure(I_f), nopass, pointer :: m_f => null()
  contains
    private
    procedure, pass(this), public :: f => get_f
  end type a_t

contains

  function get_f( this ) result( f_ptr )  ! Error message here.
    class(a_t), intent(in)  :: this
    procedure(I_f), pointer :: f_ptr
    f_ptr => this%m_f                     ! Error here :-)
  end function get_f

end module m

module test
  implicit none

  type functions
  contains
    procedure, nopass :: get_pf => get_it ! Error here
  end type

  class(functions), allocatable :: f

contains

  function get_it()                      ! Error message here.
    procedure (real), pointer :: get_it
  end function

end module
