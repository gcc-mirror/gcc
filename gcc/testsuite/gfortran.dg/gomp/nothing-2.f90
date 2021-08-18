pure subroutine foo
  !$omp nothing  ! { dg-error "OpenMP directives other than SIMD or DECLARE TARGET at .1. may not appear in PURE procedures" }
end subroutine

subroutine bar
  !$omp nothing foo  ! { dg-error "Unexpected junk after .OMP NOTHING statement" }
end
