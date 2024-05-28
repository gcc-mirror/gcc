subroutine bar
!$omp atomic
 i = i + 5
end

subroutine foo
!$omp requires atomic_default_mem_order(seq_cst)
end

subroutine foobar
!$omp atomic
 i = i + 5
!$omp requires atomic_default_mem_order(acq_rel) ! { dg-error "Unexpected !.OMP REQUIRES statement" }
end
