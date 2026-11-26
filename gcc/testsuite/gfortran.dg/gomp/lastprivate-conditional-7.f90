! Check that gfc_omp_scalar_p works with ptr_alloc_ok

subroutine sub(p, n, d, od, vd, pd, ad, opd, oad)
  use iso_c_binding
  implicit none
  integer, value :: n
  logical :: p(n)
  integer, target :: i

  type(c_ptr) :: d, od, vd, pd, ad, opd, oad, s, ps, as
  optional :: od, opd, oad
  value :: vd
  pointer :: pd, opd, ps
  allocatable :: ad, oad, as

  !$omp simd lastprivate (conditional: d)  ! { dg-error "non-scalar variable 'd' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) d = c_loc(i); end do

  !$omp simd lastprivate (conditional: od)  ! { dg-error "non-scalar variable 'od' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) od = c_loc(i); end do

  !$omp simd lastprivate (conditional: vd)  ! { dg-error "non-scalar variable 'vd' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) vd = c_loc(i); end do

  !$omp simd lastprivate (conditional: pd)  ! { dg-error "non-scalar variable 'pd' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) pd = c_loc(i); end do

  !$omp simd lastprivate (conditional: ad)  ! { dg-error "non-scalar variable 'ad' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) ad = c_loc(i); end do

  !$omp simd lastprivate (conditional: opd)  ! { dg-error "non-scalar variable 'opd' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) opd = c_loc(i); end do

  !$omp simd lastprivate (conditional: oad)  ! { dg-error "non-scalar variable 'oad' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) oad = c_loc(i); end do

  !$omp simd lastprivate (conditional: s)  ! { dg-error "non-scalar variable 's' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) s = c_loc(i); end do

  !$omp simd lastprivate (conditional: ps)  ! { dg-error "non-scalar variable 'ps' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) ps = c_loc(i); end do

  !$omp simd lastprivate (conditional: as)  ! { dg-error "non-scalar variable 'as' in conditional 'lastprivate' clause" }
  do i = 1, n; if (p(i)) as = c_loc(i); end do
end

subroutine my_entry(pp, nn, aa, bb)
  use iso_c_binding
  implicit none
  logical, save :: was_5 = .false.

  integer, value :: nn
  logical :: pp(nn)
  type(c_ptr) :: aa, bb
  allocatable :: aa
  pointer :: bb

  integer, target :: j

  !$omp simd lastprivate (conditional: aa)  ! { dg-error "non-scalar variable 'aa' in conditional 'lastprivate' clause" }
  do j = 1, nn; if (pp(j)) aa = c_loc(j); end do
  
  was_5 = .true.

entry my_entry2(pp, nn, bb)
  !$omp simd lastprivate (conditional: bb)  ! { dg-error "non-scalar variable 'bb' in conditional 'lastprivate' clause" }
  do j = 1, nn; if (pp(j)) bb = c_loc(j); end do

  if (was_5) bb = c_null_ptr
end subroutine

subroutine my_entry_opt(pp, nn, cc, dd)
  use iso_c_binding
  implicit none
  logical, save :: was_5 = .false.

  integer, value :: nn
  logical :: pp(nn)
  type(c_ptr) :: cc, dd
  optional :: cc, dd ! added
  allocatable :: cc
  pointer :: dd

  integer, target :: j

  !$omp simd lastprivate (conditional: cc)  ! { dg-error "non-scalar variable 'cc' in conditional 'lastprivate' clause" }
  do j = 1, nn; if (pp(j)) cc = c_loc(j); end do
  
  was_5 = .true.

entry my_entry2_opt(pp, nn, dd)
  !$omp simd lastprivate (conditional: dd)  ! { dg-error "non-scalar variable 'dd' in conditional 'lastprivate' clause" }
  do j = 1, nn; if (pp(j)) dd = c_loc(j); end do

  if (was_5) dd = c_null_ptr
end subroutine

subroutine my_entry_opt2(pp, nn, ee, ff)
  use iso_c_binding
  implicit none
  logical, save :: was_5 = .false.

  integer, value :: nn
  logical :: pp(nn)
  type(c_ptr) :: ee, ff
  optional :: ee, ff ! no allocatable + pointer

  integer, target :: j

  !$omp simd lastprivate (conditional: ee)  ! { dg-error "non-scalar variable 'ee' in conditional 'lastprivate' clause" }
  do j = 1, nn; if (pp(j)) ee = c_loc(j); end do
  
  was_5 = .true.

entry my_entry2_opt2(pp, nn, ff)
  !$omp simd lastprivate (conditional: ff)  ! { dg-error "non-scalar variable 'ff' in conditional 'lastprivate' clause" }
  do j = 1, nn; if (pp(j)) ff = c_loc(j); end do

  if (was_5) ff = c_null_ptr
end subroutine
