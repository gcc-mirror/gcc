! Check that gfc_omp_scalar_p works with ptr_alloc_ok
!
! { dg-additional-options "-fdump-tree-original" }

subroutine sub(p, n, d, od, vd, pd, ad, opd, oad)
  implicit none
  integer, value :: n
  logical :: p(n)
  integer :: i

  integer :: d, od, vd, pd, ad, opd, oad, s, ps, as
  optional :: od, opd, oad
  value :: vd
  pointer :: pd, opd, ps
  allocatable :: ad, oad, as

  !$omp simd lastprivate (conditional: d)
  do i = 1, n; if (p(i)) d = i; end do

  !$omp simd lastprivate (conditional: od)
  do i = 1, n; if (p(i)) od = i; end do

  !$omp simd lastprivate (conditional: vd)
  do i = 1, n; if (p(i)) vd = i; end do

  !$omp simd lastprivate (conditional: pd)
  do i = 1, n; if (p(i)) pd = i; end do

  !$omp simd lastprivate (conditional: ad)
  do i = 1, n; if (p(i)) ad = i; end do

  !$omp simd lastprivate (conditional: opd)
  do i = 1, n; if (p(i)) opd = i; end do

  !$omp simd lastprivate (conditional: oad)
  do i = 1, n; if (p(i)) oad = i; end do

  !$omp simd lastprivate (conditional: s)
  do i = 1, n; if (p(i)) s = i; end do

  !$omp simd lastprivate (conditional: ps)
  do i = 1, n; if (p(i)) ps = i; end do

  !$omp simd lastprivate (conditional: as)
  do i = 1, n; if (p(i)) as = i; end do
end

subroutine my_entry(pp, nn, aa, bb)
  implicit none
  logical, save :: was_5 = .false.

  integer, value :: nn
  logical :: pp(nn)
  integer :: aa, bb
  allocatable :: aa
  pointer :: bb

  integer :: j

  !$omp simd lastprivate (conditional: aa)
  do j = 1, nn; if (pp(j)) aa = j; end do
  
  was_5 = .true.

entry my_entry2(pp, nn, bb)
  !$omp simd lastprivate (conditional: bb)
  do j = 1, nn; if (pp(j)) bb = j; end do

  if (was_5) bb = bb + 5
end subroutine

subroutine my_entry_opt(pp, nn, cc, dd)
  implicit none
  logical, save :: was_5 = .false.

  integer, value :: nn
  logical :: pp(nn)
  integer :: cc, dd
  optional :: cc, dd ! added
  allocatable :: cc
  pointer :: dd

  integer :: j

  !$omp simd lastprivate (conditional: cc)
  do j = 1, nn; if (pp(j)) cc = j; end do
  
  was_5 = .true.

entry my_entry2_opt(pp, nn, dd)
  !$omp simd lastprivate (conditional: dd)
  do j = 1, nn; if (pp(j)) dd = j; end do

  if (was_5) dd = dd + 5
end subroutine

subroutine my_entry_opt2(pp, nn, ee, ff)
  implicit none
  logical, save :: was_5 = .false.

  integer, value :: nn
  logical :: pp(nn)
  integer :: ee, ff
  optional :: ee, ff ! no allocatable + pointer

  integer :: j

  !$omp simd lastprivate (conditional: ee)
  do j = 1, nn; if (pp(j)) ee = j; end do
  
  was_5 = .true.

entry my_entry2_opt2(pp, nn, ff)
  !$omp simd lastprivate (conditional: ff)
  do j = 1, nn; if (pp(j)) ff = j; end do

  if (was_5) ff = ff + 5
end subroutine


! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:d\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:od\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:vd\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:pd\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:ad\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:opd\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:oad\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:s\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:ps\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) lastprivate\\(conditional:as\\)" "original" } }

! { dg-final { scan-tree-dump "#pragma omp simd linear\\(j:1\\) lastprivate\\(conditional:aa\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(j:1\\) lastprivate\\(conditional:bb\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(j:1\\) lastprivate\\(conditional:cc\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(j:1\\) lastprivate\\(conditional:dd\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(j:1\\) lastprivate\\(conditional:ee\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(j:1\\) lastprivate\\(conditional:ff\\)" "original" } }
