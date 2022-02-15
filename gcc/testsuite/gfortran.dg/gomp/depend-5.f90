! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! Check that depobj is correctly dereferenced in the depend clause.

subroutine foo(dss, dsp, dsa, daa, daaa, daap, doss, dosp, dosa, doaa, doaaa, doaap)
  !use omp_lib
  use iso_c_binding, only: c_intptr_t
  implicit none (type, external)
  integer, parameter :: omp_depend_kind = 2*c_intptr_t
  integer(omp_depend_kind) :: ss, sp, sa, aa(4), aaa(:), aap(:)
  integer(omp_depend_kind) :: dss, dsp, dsa, daa(4), daaa(:), daap(:)
  integer(omp_depend_kind) :: doss, dosp, dosa, doaa(4), doaaa(:), doaap(:)
  optional :: doss, dosp, dosa, doaa, doaaa, doaap
  allocatable :: sa, aaa, dsa, daaa, dosa, doaaa
  pointer :: sp, aap, dsp, daap, dosp, doaap

  ! Assume the depend types are initialized ...

  !$omp parallel
  !$omp single
    !$omp task depend(depobj: ss)
    !$omp end task
    !$omp task depend(depobj: sp)
    !$omp end task
    !$omp task depend(depobj: sa)
    !$omp end task
    !$omp task depend(depobj: dss)
    !$omp end task
    !$omp task depend(depobj: dsp)
    !$omp end task
    !$omp task depend(depobj: dsa)
    !$omp end task
    !$omp task depend(depobj: doss)
    !$omp end task
    !$omp task depend(depobj: dosp)
    !$omp end task
    !$omp task depend(depobj: dosa)
    !$omp end task

    !$omp task depend(depobj: aa(2))
    !$omp end task
    !$omp task depend(depobj: aaa(2))
    !$omp end task
    !$omp task depend(depobj: aap(2))
    !$omp end task
    !$omp task depend(depobj: daa(2))
    !$omp end task
    !$omp task depend(depobj: daaa(2))
    !$omp end task
    !$omp task depend(depobj: daap(2))
    !$omp end task
    !$omp task depend(depobj: doaa(2))
    !$omp end task
    !$omp task depend(depobj: doaaa(2))
    !$omp end task
    !$omp task depend(depobj: doaap(2))
    !$omp end task
  !$omp end single
  !$omp end parallel
end


! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:ss\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*sp\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*sa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*dss\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\*dsp\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\*dsa\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*doss\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\*dosp\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\*dosa\\)" 1 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:aa\\\[1\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\(\\*\\(integer\\(kind=\[0-9\]+\\)\\\[0:\\\] \\* restrict\\) aaa.data\\)\\\[aaa.offset \\+ 2\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\(integer\\(kind=\[0-9\]+\\) \\*\\) \\(aap.data \\+ \\(sizetype\\) \\(\\(aap.offset \\+ aap.dim\\\[0\\\].stride \\* 2\\) \\* aap.span\\)\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\(\\*daa\\)\\\[1\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\(\\*\\(integer\\(kind=\[0-9\]+\\)\\\[0:\\\] \\* restrict\\) daaa->data\\)\\\[daaa->offset \\+ 2\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\(integer\\(kind=\[0-9\]+\\) \\*\\) \\(daap->data \\+ \\(sizetype\\) \\(\\(daap->offset \\+ daap->dim\\\[0\\\].stride \\* 2\\) \\* daap->span\\)\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\(\\*doaa\\)\\\[1\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\(\\*\\(integer\\(kind=\[0-9\]+\\)\\\[0:\\\] \\* restrict\\) doaaa->data\\)\\\[doaaa->offset \\+ 2\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task depend\\(depobj:\\*\\(integer\\(kind=\[0-9\]+\\) \\*\\) \\(doaap->data \\+ \\(sizetype\\) \\(\\(doaap->offset \\+ doaap->dim\\\[0\\\].stride \\* 2\\) \\* doaap->span\\)\\)\\)" 1 "original" } }
