! { dg-do compile }

module m
  implicit none
  !$omp requires dynamic_allocators(.true.), reverse_offload(.false.), self_maps(.false.), unified_address(.false.), unified_shared_memory(.false.)
  !$omp requires dynamic_allocators(.false.), reverse_offload(.true.), self_maps(.true.), unified_address(.true.), unified_shared_memory(.true.)
  !$omp assumes no_openmp(.false.), no_openmp_constructs(.false.) no_openmp_routines(.false.) no_parallelism(.false.)
  !$omp assumes no_openmp(.true.), no_openmp_constructs(.true.) no_openmp_routines(.true.) no_parallelism(.true.)
  integer :: x
contains
subroutine branch1
  !$omp declare simd notinbranch(.false.)
end
subroutine branch2
  !$omp declare simd notinbranch(.true.)
end
subroutine branch3
  !$omp declare simd notinbranch(.false.)
end
subroutine branch4
  !$omp declare simd notinbranch(.true.)
end
subroutine foo
  integer :: y, r
  !$omp atomic read(.false.) update(.false.) write(.false.) acq_rel(.false.), acquire(.false.), relaxed ( .true. ), release (.false.), seq_cst(.false.) weak(.false.) capture(.false.) compare(.false.)
    x = x + 1
  !$omp atomic read(.false.) update(.true.) write(.false.) acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.true.) weak(.false.) capture( .false. ), compare( .false. )
    x = x + 1
  !$omp atomic read(.true.) update(.false.) write(.false.) acq_rel(.false.), acquire(.true.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
    y = x
  !$omp atomic read(.false.) update(.false.) write(.true.) acq_rel(.true.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
    x = y + 1
  !$omp atomic read(.false.) update(.false.) write(.true.) acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
    x = y + 1

  !$omp atomic read(.false.) update write(.false.) acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.true.) weak(.false.) capture( .false. ), compare( .false. )
    x = x + 1
  !$omp atomic read,update(.false.) write(.false.) acq_rel(.false.), acquire(.true.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
    y = x
  !$omp atomic read(.false.) update(.false.) write acq_rel(.true.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
    x = y + 1

  !$omp atomic compare(.true.) weak(.true.)
     if (x == y) x = 5

  ! Flush: relaxed(.true.) not permitted
  !$omp flush acq_rel(.true.) , acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
  !$omp flush acq_rel,         acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
  !$omp flush acq_rel(.false.), acquire(.true.), relaxed ( .false. ), release (.false.), seq_cst(.false.)
  !$omp flush acq_rel(.false.), acquire        , relaxed ( .false. ), release (.false.), seq_cst(.false.)
  !$omp flush acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release (.true.), seq_cst(.false.)
  !$omp flush acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release         , seq_cst(.false.)
  !$omp flush acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst(.true.)
  !$omp flush acq_rel(.false.), acquire(.false.), relaxed ( .false. ), release (.false.), seq_cst

  !$omp unroll full(.false.)
  do y = 1, x
  end do

  !$omp unroll full(.true.)
  do y = 1, 5
  end do
end
subroutine sub
  !$omp ordered threads(.false.),simd(.false.)
  !$omp end ordered
  !$omp ordered threads(.true.),simd(.false.)
  !$omp end ordered
  !$omp ordered threads(.false.),simd(.true.)
  !$omp end ordered
  !$omp ordered threads(.true.),simd(.true.)
  !$omp end ordered
end subroutine sub

subroutine other
  !$omp task untied(.true.)
  !$omp end task
  !$omp task untied(.false.)
  !$omp end task
end
end module


!Memorder: acq_rel, acquire, relaxed, release, seq_cst
!  atomic flush
!
!atomic:
!  read, update, write
!
!Extended atomic: capture, compare, weak  (+fail)
!  atomic
!
!requirement: [device_safesync,] dynamic_allocators, reverse_offload, self_maps, unified_address, unified_shared_memory (+ atomic_default_mem_order)
!  requires
!
!branch: inbranch, notinbranch
!  declare_simd
!
!assumptions: no_openmp, no_openmp_constructs, no_openmp_routines, no_parallelism
!  assume(s)
!
!parallelization-level: threads, simd
!  ordered
!
!full
!  unroll
!
!untied
!  task, taskloop
