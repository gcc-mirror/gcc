! { dg-do run }
!
! Tests the fix for PR89364, in which the ubound and the last element of
! shape were note returning -1 for assumed rank entities, argument
! associated with assumed size dummies.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_ass_rank_04
  implicit none
contains
  subroutine si(this)
    real :: this(4, *)
    call sa(this)
  end subroutine si
  subroutine sa(this)
    real :: this(..)
    if (rank(this) /= 2) then
       stop 1
    end if
    if (maxval(abs(shape(this) - [4,-1])) > 0) then
       stop 2
    end if
    if (ubound(this,2) /= lbound(this,2) - 2) then
       stop 3
    end if
  end subroutine sa
end module mod_ass_rank_04
program ass_rank_04
  use mod_ass_rank_04
  implicit none
  real :: y(9)
  call si(y(2))
end program ass_rank_04
