! { dg-do compile }

subroutine fn1 (x)
  integer :: x
!$omp declare simd (fn1) inbranch notinbranch uniform (x) ! { dg-error "Unclassifiable OpenMP directive" }
end subroutine fn1
subroutine fn2 (x)
!$omp declare simd (fn100)	! { dg-error "should refer to containing procedure" }
end subroutine fn2
