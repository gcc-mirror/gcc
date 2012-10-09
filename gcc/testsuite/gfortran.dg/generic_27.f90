! { dg-do run }
!
! PR 45521: [F08] GENERIC resolution with ALLOCATABLE/POINTER and PROCEDURE
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m
  implicit none
  interface testIF
    module procedure test1
    module procedure test2
  end interface
contains
  real function test1 (obj)
    real :: obj
    test1 = obj
  end function
  real function test2 (pr)
    procedure(real) :: pr
    test2 = pr(0.)
  end function
end module

program test
  use m
  implicit none
  intrinsic :: cos

  if (testIF(2.0)/=2.0) call abort()
  if (testIF(cos)/=1.0) call abort()

end program

! { dg-final { cleanup-modules "m" } }
