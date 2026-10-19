! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple -w -funsigned" }

! PR middle-end/127723

module m
  implicit none
contains
  subroutine g1(a)
    integer :: a
    !$omp declare simd simdlen(scaled(integer, 1): 1)
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-1 }
  end

  subroutine g1a(a)
    integer :: a
    !$omp declare simd simdlen(scaled(integer ( kind = 4 ), 4): 4)
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-1 }
  end

  subroutine g2(a)
    integer :: a
    !$omp declare simd simdlen(scaled(double precision, 2): 2)
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-1 }
  end

  subroutine g3(a)
    integer :: a
    !$omp declare simd simdlen(scaled(complex(8)): 4)
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-1 }
  end

  subroutine g4  (a)
    integer :: a
    !$omp declare simd simdlen(scaled(unsigned(2)): 4)
! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target *-*-* } .-1 }
  end
end

! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(integer\\(kind=4\\),1\\):1\\)\\)\\)\\)" "gimple" } }
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(integer\\(kind=4\\),4\\):4\\)\\)\\)\\)" "gimple" } }
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(real\\(kind=8\\),2\\):2\\)\\)\\)\\)" "gimple" } }
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(complex\\(kind=8\\)\\):4\\)\\)\\)\\)" "gimple" } }
! { dg-final { scan-tree-dump "__attribute__\\(\\(omp declare simd \\(simdlen\\(scaled\\(unsigned\\(kind=2\\)\\):4\\)\\)\\)\\)" "gimple" } }
