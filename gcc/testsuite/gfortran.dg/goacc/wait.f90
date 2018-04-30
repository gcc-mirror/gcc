! Ensure that ACC WAIT accept integer arguments.

subroutine foo (wqueue)
  implicit none
  integer :: wqueue, waitno
  integer, parameter :: waitp = 100

  !$acc wait (wqueue)
  !$acc wait (waitno)
  !$acc wait (waitp)
  !$acc wait (0)
end subroutine foo
