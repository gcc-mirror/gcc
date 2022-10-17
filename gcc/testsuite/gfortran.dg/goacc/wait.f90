! Ensure that ACC WAIT accept integer arguments.

! { dg-additional-options -Wuninitialized }

subroutine foo (wqueue)
  implicit none
  integer :: wqueue, waitno
  ! { dg-note {'waitno' was declared here} {} { target *-*-* } .-1 }
  integer, parameter :: waitp = 100

  !$acc wait (wqueue)
  !$acc wait (waitno)
  ! { dg-warning {'waitno' is used uninitialized} {} { target *-*-* } .-1 }
  !$acc wait (waitp)
  !$acc wait (0)
end subroutine foo
