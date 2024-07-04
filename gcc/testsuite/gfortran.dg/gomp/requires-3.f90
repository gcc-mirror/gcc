!$omp requires atomic_default_mem_order(foobar)	! { dg-error "Expected ACQ_REL, ACQUIRE, RELAXED, RELEASE or SEQ_CST for ATOMIC_DEFAULT_MEM_ORDER clause" }

!$omp requires atomic_default_mem_order(acquire)	! OK since OpenMP 5.2
!$omp requires atomic_default_mem_order(release)	! { dg-error "!.OMP REQUIRES clause 'atomic_default_mem_order\\(release\\)' specified at .1. overrides a previous 'atomic_default_mem_order\\(acquire\\)' \\(which might be through using a module\\)" }
end
