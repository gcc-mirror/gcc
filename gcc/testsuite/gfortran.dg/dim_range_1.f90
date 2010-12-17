! { dg-do compile }
! PR 44693 - check for invalid dim even in functions.
! Based on a test case by Dominique d'Humieres.
subroutine test1(esss,Ix,Iyz, n)
  real(kind=kind(1.0d0)), dimension(n), intent(out) :: esss
  real(kind=kind(1.0d0)), dimension(n,n,n) :: sp
  real(kind=kind(1.0d0)), dimension(n,n) :: Ix,Iyz
  esss = sum(Ix * Iyz, 0) ! { dg-error "is not a valid dimension index" }
  esss = sum(Ix * Iyz, 1)
  esss = sum(Ix * Iyz, 2)
  esss = sum(Ix * Iyz, 3) ! { dg-error "is not a valid dimension index" }
  sp = spread (ix * iyz, 0, n) ! { dg-error "is not a valid dimension index" }
  sp = spread (ix * iyz, 1, n)
  sp = spread (ix * iyz, 2, n)
  sp = spread (ix * iyz, 3, n)
  sp = spread (ix * iyz, 4, n) ! { dg-error "is not a valid dimension index" }
end subroutine
