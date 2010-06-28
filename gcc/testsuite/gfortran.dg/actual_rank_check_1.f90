! { dg-do compile }
! Test the fix for PR40158, where the errro message was not clear about scalars.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
  implicit none
  integer :: i(4,5),j
  i = 0
  call sub1(i)
  call sub1(j)  ! { dg-error "rank-1 and scalar" }
  call sub2(i)  ! { dg-error "scalar and rank-2" }
  call sub2(j)
  print '(5i0)', i
contains
  subroutine sub1(i1)
    integer :: i1(*)
    i1(1) = 2
  end subroutine sub1
  subroutine sub2(i2)
    integer :: i2
    i2 = 2
  end subroutine sub2
end
