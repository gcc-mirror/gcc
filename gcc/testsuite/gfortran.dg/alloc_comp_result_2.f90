! Tests the fix for PR40440, in which gfortran tried to deallocate
! the allocatable components of the actual argument of CALL SUB
!
! Contributed by Juergen Reuter <juergen.reuter@desy.de>
! Reduced testcase from Tobias Burnus  <burnus@gcc.gnu.org>
!
  implicit none
  type t
    integer, allocatable :: A(:)
  end type t
  type (t) :: arg
  arg = t ([1,2,3])
  call sub (func (arg))
contains
  function func (a)
    type(t), pointer :: func
    type(t), target :: a
    integer, save :: i = 0
    if (i /= 0) STOP 1! multiple calls would cause this abort
    i = i + 1
    func => a
  end function func
  subroutine sub (a)
    type(t), intent(IN), target :: a
    if (any (a%A .ne. [1,2,3])) STOP 2
  end subroutine sub
end
