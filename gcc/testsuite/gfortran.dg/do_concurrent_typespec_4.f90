! { dg-do run }
!
! PR 125747
! Deeper nesting of DO CONCURRENT constructs that all reuse the
! index-name 'i' with type-specs, each shadowing the same outer-scope
! symbol.  Exercises three levels with three distinct kinds, and a
! non-monotonic reuse where the innermost construct repeats the
! outermost construct's type-spec.

program p
  implicit none
  integer :: total1, total2

  ! Three levels, three distinct kinds (default, kind=2, kind=8).
  total1 = 0
  do concurrent (integer :: i = 1:2) reduce(+:total1)
    do concurrent (integer(2) :: i = 1:2) reduce(+:total1)
      do concurrent (integer(8) :: i = 1:2) reduce(+:total1)
        total1 = total1 + int (i, kind (total1))
      end do
    end do
  end do
  if (total1 /= 12) stop 1

  ! Innermost construct reuses the outermost construct's type-spec.
  total2 = 0
  do concurrent (integer :: i = 1:2) reduce(+:total2)
    do concurrent (integer(2) :: i = 1:2) reduce(+:total2)
      do concurrent (integer :: i = 1:2) reduce(+:total2)
        total2 = total2 + i
      end do
    end do
  end do
  if (total2 /= 12) stop 2
end program p
