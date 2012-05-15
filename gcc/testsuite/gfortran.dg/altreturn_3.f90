! { dg-do run }
! { dg-options "-std=legacy" }
!
! Tests the fix for PR30236, which was due to alternate returns
! in generic interfaces causing a segfault.  They now work
! correctly.
!
! Contributed by Brooks Moses <brooks@gcc.gnu.org>
!
module arswitch
  implicit none
  interface gen
    module procedure with
    module procedure without
  end interface
contains
  subroutine with(i,*)
    integer i
    if (i>0) then
      i = -1
      return 1
    else
      i = -2
      return
    end if
  end subroutine
  subroutine without()
    return
  end subroutine
end module

program test
  use arswitch
  implicit none
  integer :: i = 0
  call gen (i, *10)
  if (i /= -2) call abort ()
  i = 2
  call gen (i, *20)
 10 continue
  call abort()
 20 continue
  if (i /= -1) call abort ()
end
