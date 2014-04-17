! { dg-do run }
!
! try to provoke class name clashes in gfc_build_class_symbol
!
module test_module

  implicit none

  type, public :: test_p
    private
    class (test_p), pointer :: next => null()
  end type test_p

  type, public :: test
!   Error in "call do_it (x)" below:
!   Type mismatch in argument 'x' at (1); passed CLASS(test_p) to CLASS(test)
    class (test), pointer :: next => null()
  end type test

contains

  subroutine do_it (x)
    class (test_p), target :: x

    x%next => x
    return
  end subroutine do_it

end module test_module

use test_module

  implicit none
  class (test_p), pointer :: x

  allocate (x)
  call do_it (x)
  deallocate (x)
end
