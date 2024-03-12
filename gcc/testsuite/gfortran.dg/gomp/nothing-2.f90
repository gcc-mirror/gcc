pure subroutine foo
  !$omp nothing
end subroutine

subroutine bar
  !$omp nothing foo  ! { dg-error "Unexpected junk after .OMP NOTHING statement" }
end
