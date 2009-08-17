! { dg-do compile }
! Test the fix for PR41062, in which an ICE would ensue because
! of confusion between the two 'one's in the creation of module
! debug info.
!
! Reported by Norman S. Clerman <clerman@fuse.net>
! Reduced testcase by Tobias Burnus <burnus@gcc.gnu.org>
!
module m1
   interface one  ! GENERIC "one"
     module procedure one1
   end interface
contains
  subroutine one1()
    call abort
  end subroutine one1
end module m1

module m2
use m1, only : one  ! USE generic "one"
contains
  subroutine two()
    call one()  ! Call internal "one"
  contains
    subroutine one() ! Internal "one"
      print *, "m2"
    end subroutine one
  end subroutine two
end module m2

  use m2
  call two
end
! { dg-final { cleanup-modules "m1 m2" } }
