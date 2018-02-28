! { dg-do run }
!
! PR 40646: [F03] array-valued procedure pointer components
!
! Original test case by Charlie Sharpsteen <chuck@sharpsteen.net>
! Modified by Janus Weil <janus@gcc.gnu.org>

module bugTestMod
  implicit none
  type:: boundTest
    procedure(returnMat), pointer, nopass:: test
  end type boundTest
contains
  function returnMat( a, b ) result( mat )
    integer:: a, b
    double precision, dimension(a,b):: mat 
    mat = 1d0
  end function returnMat
end module bugTestMod

program bugTest
  use bugTestMod
  implicit none
  type( boundTest ):: testObj
  double precision, dimension(2,2):: testCatch
  testObj%test => returnMat
  testCatch = testObj%test(2,2)
  print *,testCatch
  if (sum(testCatch)/=4) STOP 1
  print *,testObj%test(3,3)
  if (sum(testObj%test(3,3))/=9) STOP 2
end program bugTest
