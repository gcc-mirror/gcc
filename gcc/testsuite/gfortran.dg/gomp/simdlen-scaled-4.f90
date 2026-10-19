! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple -w" }

! PR middle-end/127723

module m
  implicit none
  interface
    subroutine fvarInt3(x); integer, value :: x; end
    subroutine fvarInt2(x); integer, value :: x; end
    subroutine fvarInt0(x); integer, value :: x; end
    subroutine fvar0(x); integer, value :: x; end
  end interface
contains
subroutine fbase (x)
  integer, value :: x
  !$omp declare_variant(fvarInt3) match(construct={parallel,simd(simdlen(scaled(integer,2): 8))})
  !$omp declare_variant(fvarInt2) match(construct={simd(simdlen(scaled(integer,2): 8))})
  !$omp declare_variant(fvarInt0) match(construct={simd(simdlen(scaled(integer): 8))})
  !$omp declare_variant(fvar0) match(construct={simd(simdlen(4))})
end
end

subroutine f()
  use m
  implicit none
  integer :: i

  !$omp simd simdlen(scaled(integer,2): 8)
  do i = 1, 1
    call fbase (1); ! -> fvarInt2
  end do

  !$omp parallel
  block
  !$omp simd simdlen(scaled(integer,2): 8)
    do i = 1, 1
      call fbase (11); ! -> fvarInt2
    end do
  end block


  !$omp simd simdlen(scaled(integer): 8)
  do i = 1, 1
    call fbase (2); ! -> fvarInt0
  end do

  !$omp simd simdlen(4)
  do i = 1, 1
    call fbase (3); ! -> fvar0
  end do

  !$omp simd simdlen(8)
  do i = 1, 1
    call fbase (4); ! -> fbase
  end do
end subroutine

! Once implemented remove ...
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 17 }
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 18 }
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 19 }
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 29 }
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 36 }
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } 43 }

! ... and the following XFAIL should work ... (it fails because the clause is removed)
! { dg-final { scan-tree-dump-times "fvarint2 \\(1\\);" 1 "gimple" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-times "fvarint3 \\(11\\);" 1 "gimple" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-times "fvarint0 \\(2\\);" 1 "gimple" { xfail *-*-* } } }

! { dg-final { scan-tree-dump-times "fvar0 \\(3\\);" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "fbase \\(4\\);" 1 "gimple" } }

! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare variant base \\(fvar0 match construct = {simd \\(simdlen\\(4\\)\\)}\\), omp declare variant base \\(fvarint0 match construct = {simd \\(simdlen\\(scaled\\(integer\\(kind=4\\)\\):8\\)\\)}\\), omp declare variant base \\(fvarint2 match construct = {simd \\(simdlen\\(scaled\\(integer\\(kind=4\\),2\\):8\\)\\)}\\), omp declare variant base \\(fvarint3 match construct = {parallel, simd \\(simdlen\\(scaled\\(integer\\(kind=4\\),2\\):8\\)\\)}\\)\\)\\)" "gimple" } }
