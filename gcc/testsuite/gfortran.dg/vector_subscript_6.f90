! { dg-do compile }
! { dg-options "-fdump-tree-original" }

subroutine test0(esss,Ix, e_x)
  real(kind=kind(1.0d0)), dimension(:), intent(out) :: esss
  real(kind=kind(1.0d0)), dimension(:), intent(in) :: Ix
  integer(kind=kind(1)), dimension(:), intent(in) :: e_x
  esss = Ix(e_x)
end subroutine

subroutine test1(esss,Ix, e_x)
  real(kind=kind(1.0d0)), dimension(:), intent(out) :: esss
  real(kind=kind(1.0d0)), dimension(:), intent(in) :: Ix
  integer(kind=4), dimension(:), intent(in) :: e_x
  esss = Ix(e_x)
end subroutine

subroutine test2(esss,Ix, e_x)
  real(kind=kind(1.0d0)), dimension(:), intent(out) :: esss
  real(kind=kind(1.0d0)), dimension(:), intent(in) :: Ix
  integer(kind=8), dimension(:), intent(in) :: e_x
  esss = Ix(e_x)
end subroutine

subroutine test3(esss,Ix,Iyz, e_x, ii_ivec)
  real(kind=kind(1.0d0)), dimension(:), intent(out) :: esss
  real(kind=kind(1.0d0)), dimension(:), intent(in) :: Ix,Iyz
  integer(kind=kind(1)), dimension(:), intent(in) :: e_x,ii_ivec
  esss = esss + Ix(e_x) * Iyz(ii_ivec)
end subroutine

! { dg-final { scan-tree-dump-not "malloc" "original" } }
! { dg-final { cleanup-tree-dump "original" } }
