! { dg-do compile }
! { dg-options "-std=gnu" }
! See altreturn_9_0.f90
subroutine sub(i, *, j)
  if (i == 10 .and. j == 20) return 1
  return
end subroutine sub
