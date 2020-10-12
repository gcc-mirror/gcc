!$omp requires atomic_default_mem_order(acquire)	! { dg-error "Expected SEQ_CST, ACQ_REL or RELAXED for ATOMIC_DEFAULT_MEM_ORDER clause" }
!$omp requires atomic_default_mem_order(release)	! { dg-error "Expected SEQ_CST, ACQ_REL or RELAXED for ATOMIC_DEFAULT_MEM_ORDER clause" }
!$omp requires atomic_default_mem_order(foobar)	! { dg-error "Expected SEQ_CST, ACQ_REL or RELAXED for ATOMIC_DEFAULT_MEM_ORDER clause" }
end
