! { dg-do compile }

module m
  implicit none
  !$omp requires dynamic_allocators(.false.), reverse_offload(.false.), self_maps(.false.), unified_address(.false.), unified_shared_memory(.false.) ! { dg-error "Expected UNIFIED_ADDRESS, UNIFIED_SHARED_MEMORY, SELF_MAPS, DYNAMIC_ALLOCATORS, REVERSE_OFFLOAD, or ATOMIC_DEFAULT_MEM_ORDER clause" }
  !$omp requires self_maps(.true.), self_maps(.true.)  ! { dg-error "Duplicated 'self_maps' clause" }
  !$omp requires unified_address(.false.), unified_address(.false.)  ! { dg-error "Duplicated 'unified_address' clause" }
  !$omp requires reverse_offload(.false.), reverse_offload(.true.)  ! { dg-error "Duplicated 'reverse_offload' clause" }
  !$omp requires self_maps, self_maps(.true.)  ! { dg-error "Duplicated 'self_maps' clause" }
  !$omp requires self_maps(.true.), self_maps  ! { dg-error "Duplicated 'self_maps' clause" }
  !$omp requires self_maps, self_maps  ! { dg-error "Duplicated 'self_maps' clause" }
  integer :: x
contains
subroutine branch1
  !$omp declare simd  inbranch(.false.) notinbranch(.false.) ! { dg-error "Duplicated branch clause: unexpected 'notinbranch' clause" }
end
subroutine branch2
  !$omp declare simd  inbranch(.false.) notinbranch(.true.) ! { dg-error "Duplicated branch clause: unexpected 'notinbranch' clause" }
end
subroutine branch3
  !$omp declare simd notinbranch(.false.) inbranch(.true.) ! { dg-error "Duplicated branch clause: unexpected 'inbranch' clause" }
end
subroutine branch4
  !$omp declare simd notinbranch(.true.) inbranch(.true.)  ! { dg-error "Duplicated branch clause: unexpected 'inbranch' clause" }
end
subroutine foo
  integer :: y, r
  !$omp atomic read(.false.) update write(.true.)  ! { dg-error "Duplicated atomic clause: unexpected 'write' clause" }
    x = x + 1
  !$omp atomic read(.false.) update(.true.) write(.true.)  ! { dg-error "Duplicated atomic clause: unexpected 'write' clause" }
    x = x + 1
  !$omp atomic write(.false.) write(.true.)
    y = x
  !$omp atomic write write(.false.)
    y = x
  !$omp atomic write write(.true.)  ! { dg-error "Duplicated atomic clause: unexpected 'write' clause" }
    y = x

  !$omp atomic acq_rel(.false.), acquire(.false.), relaxed ( .true. ), release ! { dg-error "Duplicated memory-order clause: unexpected 'release' clause" }
    x = x + 1

  ! Flush: relaxed(.true.) not permitted
  !$omp flush(y), acq_rel(.true.) ! { dg-error "List specified together with memory order clause in FLUSH directive" }
  !$omp flush relaxed ! { dg-error "Expected SEQ_CST, AQC_REL, RELEASE, or ACQUIRE" }
  !$omp flush
  !$omp flush other ! { dg-error "Expected SEQ_CST, AQC_REL, RELEASE, or ACQUIRE" }

  !$omp flush acq_rel(.true.), acquire(.true.) ! { dg-error "Duplicated memory-order clause: unexpected 'acquire' clause" }

  !$omp unroll full(.true.) full(.false.) ! { dg-error "Duplicated 'full' clause" }
  do y = 1, x
  end do

  !$omp unroll full(.true.) full(.true.) ! { dg-error "Duplicated 'full' clause" }
  do y = 1, 5
  end do

  !$omp unroll full(  ! { dg-error "Expected '\\( const-logical-expr \\)'" }
  do y = 1, 5; end do

  !$omp unroll full(1  ! { dg-error "Expected '\\( const-logical-expr \\)'" }
  do y = 1, 5; end do

  !$omp unroll full(x > 0) ! { dg-error "Expected '\\( const-logical-expr \\)'" }
  do y = 1, 5; end do

  !$omp unroll full(5) ! { dg-error "Expected '\\( const-logical-expr \\)'" }
  do y = 1, 5; end do

  !$omp unroll full([.true.]) ! { dg-error "Expected '\\( const-logical-expr \\)'" }
  do y = 1, 5; end do
end
end module
