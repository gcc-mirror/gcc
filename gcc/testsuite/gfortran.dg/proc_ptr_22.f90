! { dg-do run }
!
! PR 40646: [F03] array-valued procedure pointer components
!
! Original test case by Charlie Sharpsteen <chuck@sharpsteen.net>
! Modified by Janus Weil <janus@gcc.gnu.org>

module bugTestMod
  implicit none
  procedure(returnMat), pointer :: pp2
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
  procedure(returnMat), pointer :: pp
  pp => returnMat
  if (sum(pp(2,2))/=4) call abort()
  pp2 => returnMat
  if (sum(pp2(3,2))/=6) call abort()
end program bugTest

! { dg-final { cleanup-modules "bugtestmod" } }

