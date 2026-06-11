! { dg-do run }
!
! PR 125747
! A nested DO CONCURRENT that re-shadows the same index-name 'i' as its
! enclosing construct, with a different type-spec.  The outer construct's
! body-substitution (i -> its shadow) must not also rewrite references to
! 'i' inside the nested construct's body, since the nested construct
! shadows 'i' itself and performs its own substitution.

program p
  implicit none
  integer :: s1, s2, total

  ! Establish 'i' as INTEGER(4) via a prior sibling construct.
  s1 = 0
  do concurrent (integer :: i = 1:3) reduce(+:s1)
    s1 = s1 + i
  end do

  s2 = 0
  total = 0
  do concurrent (integer :: i = 1:2) reduce(+:s2)
    s2 = s2 + i
    do concurrent (integer(2) :: i = 1:3) reduce(+:total)
      total = total + int (i, kind (total))
    end do
  end do

  if (s1 /= 6) stop 1
  if (s2 /= 3) stop 2
  if (total /= 12) stop 3
end program p
