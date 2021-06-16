! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-gimple" }
!
! PR fortran/92568
!
implicit none
  type t
  end type t

  integer :: ii
  integer :: arr(5)
  integer, allocatable :: aii, aarr(:)
  integer, pointer :: pii, parr(:)

  character :: str1, str1arr(5), str1a, str1aarr(:), str1p, str1parr(:)
  character(len=5) :: str5, str5arr(5), str5a, str5aarr(:), str5p, str5parr(:)
  character(len=:) :: strXa, strXaarr(:), strXp, strXparr(:)
  allocatable :: str1a, str1aarr, str5a, str5aarr, strXa, strXaarr
  pointer :: str1p, str1parr, str5p, str5parr, strXp, strXparr

  type(t) :: dt, dtarr(5), dta, dtaarr(:), dtp, dtparr(:)
  allocatable :: dta, dtaarr
  pointer :: dtp, dtparr

  allocate(aii, aarr(5), str1a, str1aarr(5), dta, dtparr(5))
  allocate(pii, parr(5), str1p, str1parr(5), dtp, dtparr(5))
  allocate(character(len=7) :: strXa, strXaarr(5), strXp, strXparr(5))


  !$omp target defaultmap ( none )  &
  !$omp&  map(tofrom: ii, arr, aii, aarr, pii, parr)  &
  !$omp&  map(tofrom: str1, str1arr, str1a, str1aarr, str1p, str1parr)  &
  !$omp&  map(tofrom: str5, str5arr, str5a, str5aarr, str5p, str5parr)  &
  !$omp&  map(tofrom: strXa, strXaarr, strXp, strXparr)  &
  !$omp&  map(tofrom: dt, dtarr, dta, dtaarr, dtp, dtparr)
    ii = 42; arr = 42; aii = 42; aarr = 42; pii = 42; parr = 42
    str1 = ""; str1arr = ""; str1a = ""; str1aarr = ""; str1p = ""; str1parr = ""
    str5 = ""; str5arr = ""; str5a = ""; str5aarr = ""; str5p = ""; str5parr = ""
    strXa = ""; strXaarr = ""; strXp = ""; strXparr = ""
    dt = t(); dtarr = t(); dta = t(); dtaarr = t(); dtp = t(); dtparr = t()
  !$omp end target


  !$omp target defaultmap(none : scalar)  defaultmap(none : aggregate)  &
  !$omp&       defaultmap(none : allocatable) defaultmap(none : pointer) &
  !$omp&  map(alloc: ii, arr, aii, aarr, pii, parr)  &
  !$omp&  map(alloc: str1, str1arr, str1a, str1aarr, str1p, str1parr)  &
  !$omp&  map(alloc: str5, str5arr, str5a, str5aarr, str5p, str5parr)  &
  !$omp&  map(alloc: strXa, strXaarr, strXp, strXparr)  &
  !$omp&  map(alloc: dt, dtarr, dta, dtaarr, dtp, dtparr)
    ii = 42; arr = 42; aii = 42; aarr = 42; pii = 42; parr = 42
    str1 = ""; str1arr = ""; str1a = ""; str1aarr = ""; str1p = ""; str1parr = ""
    str5 = ""; str5arr = ""; str5a = ""; str5aarr = ""; str5p = ""; str5parr = ""
    strXa = ""; strXaarr = ""; strXp = ""; strXparr = ""
    dt = t(); dtarr = t(); dta = t(); dtaarr = t(); dtp = t(); dtparr = t()
  !$omp end target
end

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:.* defaultmap\\(none\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(alloc:.* defaultmap\\(none:scalar\\) defaultmap\\(none:aggregate\\) defaultmap\\(none:allocatable\\) defaultmap\\(none:pointer\\)" 1 "original" } }
