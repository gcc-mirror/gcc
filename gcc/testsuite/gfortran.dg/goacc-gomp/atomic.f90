! { dg-do compile } */
! { dg-additional-options "-fdump-tree-original" } */

subroutine foo
  !$omp requires atomic_default_mem_order(acq_rel)
  integer :: i, v

  !$omp atomic read
  i = v

  !$acc atomic read
  i = v

  !$omp atomic write
  i = v

  !$acc atomic write
  i = v

  !$omp atomic update
  i = i + 1

  !$acc atomic update
  i = i + 1

  !$omp atomic capture
    i = i + 1
    v = i
  !$omp end atomic

  !$acc atomic capture
    i = i + 1
    v = i
  !$acc end atomic

  ! Valid in C/C++ since OpenACC 2.5 but not in Fortran:
  ! !$acc atomic update capture
  !   i = i + 1
  !   v = i
  ! !$acc end atomic
end

! { dg-final { scan-tree-dump-times "i = #pragma omp atomic read acquire" 1 "original" } }
! { dg-final { scan-tree-dump-times "i = #pragma omp atomic read relaxed" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic release" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic relaxed" 2 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture acq_rel" 1  "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture relaxed" 1 "original" } }
