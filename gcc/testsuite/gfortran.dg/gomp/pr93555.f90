! PR middle-end/93555
! { dg-do compile }

subroutine foo
  !$omp declare simd(foo)
  !$omp declare simd(foo) inbranch
end
subroutine bar
  !$omp declare simd(bar) inbranch
  !$omp declare simd(bar)
end
