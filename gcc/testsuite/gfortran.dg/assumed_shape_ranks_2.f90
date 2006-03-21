! { dg-do run }
! Tests the fix for the regression PR26716.
! Test contributed by Martin Reinecke  <martin@mpa-garching.mpg.de>
!
module mod1
  implicit none

  interface foo
     module procedure foo1, foo2
  end interface

contains

  subroutine foo1(bar, i)
    real bar
    integer i
    i = 1
   end subroutine

  subroutine foo2(bar, i)
    real bar(3)
    integer i
    i = 2
  end subroutine

end module mod1

  use mod1
  implicit none

  real bar(3)
  integer i

  i = 0
  call foo (1e0, i)
  if (i .ne. 1) call abort ()

  i = 0
  call foo (bar(1), i)
  if (i .ne. 1) call abort ()

  i = 0
  call foo (bar, i)
  if (i .ne. 2) call abort ()
end

! { dg-final { cleanup-modules "mod1" } }
