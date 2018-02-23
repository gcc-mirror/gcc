! { dg-do run }
! Tests the fix for PR40646 in which the assignment would cause an ICE.
!
! Contributed by Charlie Sharpsteen  <chuck@sharpsteen.net>
! http://gcc.gnu.org/ml/fortran/2009-07/msg00010.html
! and reported by Tobias Burnus  <burnus@gcc,gnu.org>
!
module bugTestMod
  implicit none
  type:: boundTest
  contains
    procedure, nopass:: test => returnMat
  end type boundTest
contains
  function returnMat( a, b ) result( mat )
    integer:: a, b, i
    double precision, dimension(a,b):: mat
    mat = dble (reshape ([(i, i = 1, a * b)],[a,b])) 
    return
  end function returnMat
end module bugTestMod

program bugTest
  use bugTestMod
  implicit none
  integer i
  double precision, dimension(2,2):: testCatch
  type( boundTest ):: testObj
  testCatch = testObj%test(2,2)  ! This would cause an ICE
  if (any (testCatch .ne. dble (reshape ([(i, i = 1, 4)],[2,2])))) STOP 1
end program bugTest
