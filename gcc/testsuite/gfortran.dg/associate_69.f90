! { dg-do compile }
! { dg-options "-Og -Wuninitialized -Wmaybe-uninitialized -fdump-tree-optimized" }
!
! PR fortran/115700 - Bogus warning for associate with assumed-length character array
! This testcase checks for the suppression of the bogus error and associate_70 for
! correct results.
!
subroutine mvce(x)
  implicit none
  character(len=*), dimension(:), intent(in)  :: x
  integer :: i
  i = len(x)

  associate (tmp1 => x)
    if (len (tmp1) /= len (x)) stop 1
  end associate

  associate (tmp2 => x(1:))
    if (len (tmp2) /= len (x)) stop 2
  end associate

  associate (tmp3 => x(1:)(:))
    if (len (tmp3) /= len (x)) stop 3
  end associate

  associate (tmp4 => x(:)(1:))
    if (len (tmp4) /= len (x)) stop 4
  end associate

  associate (tmp5 => x(1:)(1:))
    if (len (tmp5) /= len (x)) stop 5
  end associate
end

! { dg-final { scan-tree-dump-not " \\.tmp" "optimized" } }
