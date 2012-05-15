! { dg-do run }
!
! PR 45290: [F08] pointer initialization
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m
 integer, target  :: t1          ! SAVE is implicit
 integer, pointer :: p1 => t1
end module m


use m
implicit none

integer,target :: i0 = 2
integer,target,dimension(1:3) :: vec = 1

type :: t
  integer, pointer :: dpc => i0
  integer :: i = 0
end type

type (t), save, target :: u

integer, pointer :: dp => i0
integer, pointer :: dp2 => vec(2)
integer, pointer :: dp3 => u%i

dp = 5
if (i0/=5) call abort()

u%dpc = 6
if (i0/=6) call abort()

dp2 = 3
if (vec(2)/=3) call abort()

dp3 = 4
if (u%i/=4) call abort()

end 
