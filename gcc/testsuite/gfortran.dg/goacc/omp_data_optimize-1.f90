! { dg-additional-options "-fdump-tree-gimple-raw" }
! { dg-additional-options "-fopt-info-omp-all" }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_compute[variable c_compute 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_compute } to avoid
! "WARNING: dg-line var l_compute defined, but not used".
! { dg-line l_use[variable c_use 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_use } to avoid
! "WARNING: dg-line var l_use defined, but not used".

module globals
  use ISO_C_BINDING
  implicit none
  integer :: opt_1_gvar1 = 1
  integer(C_INT), bind(C) :: opt_1_evar1
  integer :: opt_2_gvar1 = 1
  integer(C_INT), bind(C) :: opt_2_evar1
  integer :: opt_3_gvar1 = 1
  integer(C_INT), bind(C) :: opt_3_evar1
  integer :: use_1_gvar1 = 1
  integer(C_INT), bind(C) :: use_1_evar1
  integer :: use_2_gvar1 = 1
  integer(C_INT), bind(C) :: use_2_evar1
  integer :: use_2_a1(100)
  integer(C_INT), bind(C) :: lcf_1_evar2
  integer(C_INT), bind(C) :: lcf_2_evar2
  integer(C_INT), bind(C) :: lcf_3_evar2
  integer(C_INT), bind(C) :: lcf_4_evar2
  integer(C_INT), bind(C) :: lcf_5_evar2
  integer(C_INT), bind(C) :: lcf_6_evar2
  save
end module globals

subroutine opt_1 (opt_1_pvar1)
  use globals
  implicit none
  integer :: opt_1_pvar1
  integer :: opt_1_lvar1
  integer, save :: opt_1_svar1 = 3
  integer :: dummy1, dummy2, dummy3, dummy4, dummy5

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    dummy1 = opt_1_pvar1;
    dummy2 = opt_1_lvar1;

    dummy3 = opt_1_gvar1;
    dummy4 = opt_1_evar1;
    dummy5 = opt_1_svar1;
  !$acc end kernels

! Parameter is pass-by-reference
! { dg-missed {'map\(force_tofrom:\*opt_1_pvar1 \[len: [0-9]+\]\)' not optimized: \*opt_1_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-optimized {'map\(force_tofrom:opt_1_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_1_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
!
! { dg-missed {'map\(force_tofrom:opt_1_gvar1 \[len: [0-9]+\]\)' not optimized: opt_1_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_1_evar1 \[len: [0-9]+\]\)' not optimized: opt_1_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_1_svar1 \[len: 4\]\)' not optimized: opt_1_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
!
! { dg-optimized {'map\(force_tofrom:dummy1 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy1 \[len: [0-9]+\]\)' further optimized to 'private\(dummy1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy2 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy2 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy2 \[len: [0-9]+\]\)' further optimized to 'private\(dummy2\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy3 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy3 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy3 \[len: [0-9]+\]\)' further optimized to 'private\(dummy3\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy4 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy4 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy4 \[len: [0-9]+\]\)' further optimized to 'private\(dummy4\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy5 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy5 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy5 \[len: [0-9]+\]\)' further optimized to 'private\(dummy5\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine opt_1

subroutine opt_2 (opt_2_pvar1)
  use globals
  implicit none
  integer :: opt_2_pvar1
  integer :: opt_2_lvar1
  integer, save :: opt_2_svar1 = 3
  integer :: dummy1, dummy2, dummy3, dummy4, dummy5

  !$acc kernels    ! { dg-line l_compute[incr c_compute] }
    dummy1 = opt_2_pvar1;
    dummy2 = opt_2_lvar1;

    dummy3 = opt_2_gvar1;
    dummy4 = opt_2_evar1;
    dummy5 = opt_2_svar1;
  !$acc end kernels

  ! A write does not inhibit optimization.
  opt_2_pvar1 = 0;
  opt_2_lvar1 = 1;

  opt_2_gvar1 = 10;
  opt_2_evar1 = 11;
  opt_2_svar1 = 12;

! { dg-missed {'map\(force_tofrom:\*opt_2_pvar1 \[len: [0-9]+\]\)' not optimized: \*opt_2_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-optimized {'map\(force_tofrom:opt_2_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_2_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_2_gvar1 \[len: [0-9]+\]\)' not optimized: opt_2_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_2_evar1 \[len: [0-9]+\]\)' not optimized: opt_2_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_2_svar1 \[len: 4\]\)' not optimized: opt_2_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-optimized {'map\(force_tofrom:dummy1 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy1 \[len: [0-9]+\]\)' further optimized to 'private\(dummy1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy2 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy2 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy2 \[len: [0-9]+\]\)' further optimized to 'private\(dummy2\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy3 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy3 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy3 \[len: [0-9]+\]\)' further optimized to 'private\(dummy3\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy4 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy4 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy4 \[len: [0-9]+\]\)' further optimized to 'private\(dummy4\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy5 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy5 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy5 \[len: [0-9]+\]\)' further optimized to 'private\(dummy5\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine opt_2

subroutine opt_3 (opt_3_pvar1)
  use globals
  implicit none
  integer :: opt_3_pvar1
  integer :: opt_3_lvar1
  integer, save :: opt_3_svar1 = 3

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    opt_3_pvar1 = 0;
    opt_3_lvar1 = 1;

    opt_3_gvar1 = 10;
    opt_3_evar1 = 11;
    opt_3_svar1 = 12;
  !$acc end kernels

! Parameter is pass-by-reference
! { dg-missed {'map\(force_tofrom:\*opt_3_pvar1 \[len: [0-9]+\]\)' not optimized: \*opt_3_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-optimized {'map\(force_tofrom:opt_3_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_3_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:opt_3_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(opt_3_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
!
! { dg-missed {'map\(force_tofrom:opt_3_gvar1 \[len: [0-9]+\]\)' not optimized: opt_3_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_3_evar1 \[len: [0-9]+\]\)' not optimized: opt_3_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }

! { dg-missed {'map\(force_tofrom:opt_3_svar1 \[len: 4\]\)' not optimized: opt_3_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine opt_3

subroutine opt_4 ()
  implicit none
  integer, dimension(10) :: opt_4_larray1
  integer :: dummy1, dummy2

  ! TODO Fortran local arrays are addressable (and may be visable to nested
  ! functions, etc.) so they are not optimizable yet.

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    dummy1 = opt_4_larray1(4)
    dummy2 = opt_4_larray1(8)
  !$acc end kernels
 
! { dg-missed {'map\(tofrom:opt_4_larray1 \[len: [0-9]+\]\)' not optimized: opt_4_larray1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
!
! { dg-optimized {'map\(force_tofrom:dummy1 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy1 \[len: [0-9]+\]\)' further optimized to 'private\(dummy1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy2 \[len: [0-9]+\]\)' optimized to 'map\(to:dummy2 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy2 \[len: [0-9]+\]\)' further optimized to 'private\(dummy2\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine opt_4

subroutine opt_5 (opt_5_pvar1)
  implicit none
  integer, dimension(10) :: opt_5_larray1
  integer :: opt_5_lvar1, opt_5_pvar1

  opt_5_lvar1 = opt_5_pvar1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    opt_5_larray1(opt_5_lvar1) = 1
  !$acc end kernels
 
! { dg-missed {'map\(tofrom:opt_5_larray1 \[len: [0-9]+\]\)' not optimized: opt_5_larray1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
!
! { dg-optimized {'map\(force_tofrom:opt_5_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:opt_5_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine opt_5

subroutine use_1 (use_1_pvar1)
  use globals
  implicit none
  integer :: use_1_pvar1
  integer :: use_1_lvar1
  integer, save :: use_1_svar1 = 3
  integer :: s

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    use_1_pvar1 = 0;
    use_1_lvar1 = 1;

    ! FIXME: svar is optimized: should not be
    use_1_gvar1 = 10;
    use_1_evar1 = 11;
    use_1_svar1 = 12;
  !$acc end kernels

  s = 0
  s = s + use_1_pvar1
  s = s + use_1_lvar1 ! { dg-missed {\.\.\. here} "" { target *-*-* } }
  s = s + use_1_gvar1
  s = s + use_1_evar1
  s = s + use_1_svar1

! { dg-missed {'map\(force_tofrom:\*use_1_pvar1 \[len: [0-9]+\]\)' not optimized: \*use_1_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_1_lvar1 \[len: [0-9]+\]\)' not optimized: use_1_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_1_gvar1 \[len: [0-9]+\]\)' not optimized: use_1_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_1_evar1 \[len: [0-9]+\]\)' not optimized: use_1_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_1_svar1 \[len: [0-9]+\]\)' not optimized: use_1_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine use_1

subroutine use_2 (use_2_pvar1)
  use globals
  implicit none
  integer :: use_2_pvar1
  integer :: use_2_lvar1
  integer, save :: use_2_svar1 = 3
  integer :: s

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    use_2_pvar1 = 0;
    use_2_lvar1 = 1;
    use_2_gvar1 = 10;
    use_2_evar1 = 11;
    use_2_svar1 = 12;
  !$acc end kernels

  s = 0
  s = s + use_2_a1(use_2_pvar1)
  s = s + use_2_a1(use_2_lvar1) ! { dg-missed {\.\.\. here} "" { target *-*-* } }
  s = s + use_2_a1(use_2_gvar1)
  s = s + use_2_a1(use_2_evar1)
  s = s + use_2_a1(use_2_svar1)

! { dg-missed {'map\(force_tofrom:\*use_2_pvar1 \[len: [0-9]+\]\)' not optimized: \*use_2_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_2_lvar1 \[len: [0-9]+\]\)' not optimized: use_2_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_2_gvar1 \[len: [0-9]+\]\)' not optimized: use_2_gvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_2_evar1 \[len: [0-9]+\]\)' not optimized: use_2_evar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:use_2_svar1 \[len: [0-9]+\]\)' not optimized: use_2_svar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine use_2

! Optimization inhibited because of looping/control flow.

subroutine lcf_1 (lcf_1_pvar1, iter)
  use globals
  implicit none
  real :: lcf_1_pvar1
  real :: lcf_1_lvar1
  real, save :: lcf_1_svar2
  integer :: i, iter

  do i = 1, iter ! { dg-line l_use[incr c_use] }
    !$acc kernels ! { dg-line l_compute[incr c_compute] }
      lcf_1_pvar1 = 0
      lcf_1_lvar1 = 1
      lcf_1_evar2 = 2
      lcf_1_svar2 = 3
    !$acc end kernels
  end do

! { dg-missed {'map\(force_tofrom:\*lcf_1_pvar1 \[len: [0-9]+\]\)' not optimized: \*lcf_1_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_1_lvar1 \[len: [0-9]+\]\)' not optimized: lcf_1_lvar1 disguised by looping/control flow...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_1_evar2 \[len: [0-9]+\]\)' not optimized: lcf_1_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_1_svar2 \[len: [0-9]+\]\)' not optimized: lcf_1_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use }
end subroutine lcf_1

subroutine lcf_2 (lcf_2_pvar1)
  use globals
  implicit none
  real :: lcf_2_pvar1
  real :: lcf_2_lvar1
  real, save :: lcf_2_svar2
  integer :: dummy

10 dummy = 1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    lcf_2_pvar1 = 0
    lcf_2_lvar1 = 1
    lcf_2_evar2 = 2
    lcf_2_svar2 = 3
  !$acc end kernels

  go to 10 ! { dg-line l_use[incr c_use] }

! { dg-missed {'map\(force_tofrom:\*lcf_2_pvar1 \[len: [0-9]+\]\)' not optimized: \*lcf_2_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_2_lvar1 \[len: [0-9]+\]\)' not optimized: lcf_2_lvar1 disguised by looping/control flow...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_2_evar2 \[len: [0-9]+\]\)' not optimized: lcf_2_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_2_svar2 \[len: [0-9]+\]\)' not optimized: lcf_2_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use }
end subroutine lcf_2

subroutine lcf_3 (lcf_3_pvar1)
  use globals
  implicit none
  real :: lcf_3_pvar1
  real :: lcf_3_lvar1
  real, save :: lcf_3_svar2
  integer :: dummy

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    lcf_3_pvar1 = 0
    lcf_3_lvar1 = 1
    lcf_3_evar2 = 2
    lcf_3_svar2 = 3
  !$acc end kernels

  ! Backward jump after kernel
10 dummy = 1
  go to 10 ! { dg-line l_use[incr c_use] }

! { dg-missed {'map\(force_tofrom:\*lcf_3_pvar1 \[len: [0-9]+\]\)' not optimized: \*lcf_3_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_3_lvar1 \[len: [0-9]+\]\)' not optimized: lcf_3_lvar1 disguised by looping/control flow...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_3_evar2 \[len: [0-9]+\]\)' not optimized: lcf_3_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_3_svar2 \[len: [0-9]+\]\)' not optimized: lcf_3_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use }
end subroutine lcf_3

subroutine lcf_4 (lcf_4_pvar1)
  use globals
  implicit none
  real :: lcf_4_pvar1
  real :: lcf_4_lvar1
  real, save :: lcf_4_svar2
  integer :: dummy

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    lcf_4_pvar1 = 0
    lcf_4_lvar1 = 1
    lcf_4_evar2 = 2
    lcf_4_svar2 = 3
  !$acc end kernels

  ! Forward jump after kernel
  go to 10
10 dummy = 1

! { dg-missed {'map\(force_tofrom:\*lcf_4_pvar1 \[len: [0-9]+\]\)' not optimized: \*lcf_4_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:lcf_4_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_4_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:lcf_4_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_4_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_4_evar2 \[len: [0-9]+\]\)' not optimized: lcf_4_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_4_svar2 \[len: [0-9]+\]\)' not optimized: lcf_4_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine lcf_4

subroutine lcf_5 (lcf_5_pvar1, lcf_5_pvar2)
  use globals
  implicit none
  real :: lcf_5_pvar1
  real :: lcf_5_pvar2
  real :: lcf_5_lvar1
  real, save :: lcf_5_svar2
  integer :: dummy

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    lcf_5_pvar1 = 0
    lcf_5_lvar1 = 1
    lcf_5_evar2 = 2
    lcf_5_svar2 = 3
  !$acc end kernels

  if (lcf_5_pvar2 > 0) then
    dummy = 1
  end if

! { dg-missed {'map\(force_tofrom:\*lcf_5_pvar1 \[len: [0-9]+\]\)' not optimized: \*lcf_5_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:lcf_5_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_5_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:lcf_5_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_5_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_5_evar2 \[len: [0-9]+\]\)' not optimized: lcf_5_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_5_svar2 \[len: [0-9]+\]\)' not optimized: lcf_5_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine lcf_5

subroutine lcf_6 (lcf_6_pvar1, lcf_6_pvar2)
  use globals
  implicit none
  real :: lcf_6_pvar1
  real :: lcf_6_pvar2
  real :: lcf_6_lvar1
  real, save :: lcf_6_svar2
  integer :: dummy

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    lcf_6_pvar1 = 0
    lcf_6_lvar1 = 1
    lcf_6_evar2 = 2
    lcf_6_svar2 = 3
  !$acc end kernels

  dummy = merge(1,0, lcf_6_pvar2 > 0)

! { dg-missed {'map\(force_tofrom:\*lcf_6_pvar1 \[len: [0-9]+\]\)' not optimized: \*lcf_6_pvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:lcf_6_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:lcf_6_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:lcf_6_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(lcf_6_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_6_evar2 \[len: [0-9]+\]\)' not optimized: lcf_6_evar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:lcf_6_svar2 \[len: [0-9]+\]\)' not optimized: lcf_6_svar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine lcf_6

subroutine priv_1 ()
  implicit none
  integer :: priv_1_lvar1, priv_1_lvar2, priv_1_lvar3, priv_1_lvar4
  integer :: priv_1_lvar5, priv_1_lvar6, dummy

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    ! { dg-message {note: beginning 'Graphite' part in OpenACC 'kernels' region} "" { target *-*-* } .+1 } */
    priv_1_lvar1 = 1
    dummy = priv_1_lvar2

    if (priv_1_lvar2 > 0) then
        priv_1_lvar3 = 1
    else
        priv_1_lvar3 = 2
    end if

    priv_1_lvar5 = priv_1_lvar3

    if (priv_1_lvar2 > 0) then
        priv_1_lvar4 = 1
        dummy = priv_1_lvar4
    end if
  !$acc end kernels

! { dg-optimized {'map\(force_tofrom:priv_1_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:priv_1_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:priv_1_lvar2 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar2 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-bogus {'map\(to:priv_1_lvar2 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar2\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:priv_1_lvar3 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar3 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:priv_1_lvar3 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar3\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:priv_1_lvar4 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar4 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:priv_1_lvar4 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar4\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:priv_1_lvar5 \[len: [0-9]+\]\)' optimized to 'map\(to:priv_1_lvar5 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:priv_1_lvar5 \[len: [0-9]+\]\)' further optimized to 'private\(priv_1_lvar5\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(force_tofrom:dummy \[len: [0-9]+\]\)' optimized to 'map\(to:dummy \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:dummy \[len: [0-9]+\]\)' further optimized to 'private\(dummy\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine priv_1

subroutine multiple_kernels_1 ()
  implicit none
  integer :: multiple_kernels_1_lvar1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    multiple_kernels_1_lvar1 = 1
  !$acc end kernels

  !$acc kernels ! { dg-line l_use[incr c_use] }
    multiple_kernels_1_lvar1 = multiple_kernels_1_lvar1 + 1
  !$acc end kernels

! { dg-missed {'map\(force_tofrom:multiple_kernels_1_lvar1 \[len: [0-9]+\]\)' not optimized: multiple_kernels_1_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use }

! { dg-optimized {'map\(force_tofrom:multiple_kernels_1_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:multiple_kernels_1_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_use$c_use }
end subroutine multiple_kernels_1

subroutine multiple_kernels_2 ()
  implicit none
  integer :: multiple_kernels_2_lvar1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    multiple_kernels_2_lvar1 = 1
  !$acc end kernels

  !$acc parallel
    multiple_kernels_2_lvar1 = multiple_kernels_2_lvar1 + 1 ! { dg-line l_use[incr c_use] }
  !$acc end parallel

! { dg-missed {'map\(force_tofrom:multiple_kernels_2_lvar1 \[len: [0-9]+\]\)' not optimized: multiple_kernels_2_lvar1 used...} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {\.\.\. here} "" { target *-*-* } l_use$c_use }
end subroutine multiple_kernels_2

integer function ref_1 ()
  implicit none
  integer, target :: ref_1_lvar1
  integer, target :: ref_1_lvar2
  integer, pointer :: ref_1_ref1

  ref_1_ref1 => ref_1_lvar1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    ref_1_lvar1 = 1
    ! FIXME: currently considered unsuitable; but could be optimized
    ref_1_lvar2 = 2
  !$acc end kernels

  ref_1 = ref_1_ref1

! { dg-missed {'map\(force_tofrom:ref_1_lvar1 \[len: [0-9]+\]\)' not optimized: ref_1_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:ref_1_lvar2 \[len: [0-9]+\]\)' not optimized: ref_1_lvar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end function ref_1

integer function ref_2 ()
  implicit none
  integer, target :: ref_2_lvar1
  integer, target :: ref_2_lvar2
  integer, pointer :: ref_2_ref1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    ref_2_lvar1 = 1
    ! FIXME: currently considered unsuitable, but could be optimized
    ref_2_lvar2 = 2
  !$acc end kernels

  ref_2_ref1 => ref_2_lvar1
  ref_2 = ref_2_ref1

! { dg-missed {'map\(force_tofrom:ref_2_lvar1 \[len: [0-9]+\]\)' not optimized: ref_2_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:ref_2_lvar2 \[len: [0-9]+\]\)' not optimized: ref_2_lvar2 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end function ref_2

subroutine ref_3 ()
  implicit none
  integer, target :: ref_3_lvar1
  integer, pointer :: ref_3_ref1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    ref_3_ref1 => ref_3_lvar1

    ! FIXME: currently considered unsuitable, but could be optimized
    ref_3_lvar1 = 1
  !$acc end kernels

! { dg-missed {'map\(force_tofrom:\*ref_3_ref1 \[len: [0-9]+\]\)' not optimized: \*ref_3_ref1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:ref_3_lvar1 \[len: [0-9]+\]\)' not optimized: ref_3_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine ref_3

subroutine ref_4 ()
  implicit none
  integer, target :: ref_4_lvar1
  integer, pointer :: ref_4_ref1

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
    ref_4_ref1 => ref_4_lvar1

    ! FIXME: currently considered unsuitable, but could be optimized
    ref_4_ref1 = 1
  !$acc end kernels

! { dg-missed {'map\(force_tofrom:\*ref_4_ref1 \[len: [0-9]+\]\)' not optimized: \*ref_4_ref1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
! { dg-missed {'map\(force_tofrom:ref_4_lvar1 \[len: [0-9]+\]\)' not optimized: ref_4_lvar1 is unsuitable for privatization} "" { target *-*-* } l_compute$c_compute }
end subroutine ref_4

subroutine conditional_1 (conditional_1_pvar1)
  implicit none
  integer :: conditional_1_pvar1
  integer :: conditional_1_lvar1

  conditional_1_lvar1 = 1

  if (conditional_1_pvar1 > 0) then
    !$acc kernels ! { dg-line l_compute[incr c_compute] }
      conditional_1_lvar1 = 2
    !$acc end kernels
  else
    conditional_1_lvar1 = 3
  end if

! { dg-optimized {'map\(force_tofrom:conditional_1_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:conditional_1_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:conditional_1_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(conditional_1_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine conditional_1

subroutine conditional_2 (conditional_2_pvar1)
  implicit none
  integer :: conditional_2_pvar1
  integer :: conditional_2_lvar1

  conditional_2_lvar1 = 1

  if (conditional_2_pvar1 > 0) then
    conditional_2_lvar1 = 3
  else
    !$acc kernels ! { dg-line l_compute[incr c_compute] }
      conditional_2_lvar1 = 2
    !$acc end kernels
  end if

! { dg-optimized {'map\(force_tofrom:conditional_2_lvar1 \[len: [0-9]+\]\)' optimized to 'map\(to:conditional_2_lvar1 \[len: [0-9]+\]\)'} "" { target *-*-* } l_compute$c_compute }
! { dg-optimized {'map\(to:conditional_2_lvar1 \[len: [0-9]+\]\)' further optimized to 'private\(conditional_2_lvar1\)'} "" { target *-*-* } l_compute$c_compute }
end subroutine conditional_2
