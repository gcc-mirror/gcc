! { dg-do run }
! See attachment to PR82173 comment 10 - reproducers/bug17-final-rank-overload-ice.f90
module mm
  type :: t(k, n)
    integer, kind :: k = 4
    integer, len :: n = 1
    real(k) :: v(n)
  contains
    final :: fin4, fin8, fin4_r1
  end type
  integer :: tally = 0
contains
  subroutine fin4 (a)
    type(t(4, *)), intent(inout) :: a
    tally = tally + 1
  end subroutine
  subroutine fin8 (a)
    type(t(8, *)), intent(inout) :: a
    tally = tally + 10
  end subroutine
  subroutine fin4_r1 (a)
    type(t(4, *)), intent(inout) :: a(:)
    tally = tally + 100
  end subroutine
end module   ! Segmentation fault here

  use mm
  call foo
!  Each final routine is called twice - assignment and leaving scope
  if (tally /= 222) stop 1 
contains
  subroutine foo
  type(t(4, 2)) :: a, b(2)
  type(t(8, 2)) :: c
  a = t(4, 2)([1.0,2.0])
  b = [t(4, 2)([1.0,2.0]), t(4, 2)([3.0,4.0])]
  c = t(8, 2)([1.0,2.0])
  end
end
