! { dg-additional-options "-fdump-tree-original" }

module relaxed
  !$omp requires atomic_default_mem_order(relaxed)
end module relaxed

module seq
  !$omp requires atomic_default_mem_order(seq_cst)
end module seq

module acq
  !$omp requires atomic_default_mem_order(acq_rel)
end module acq

subroutine sub1
  !$omp atomic  ! <= relaxed
  i1 = i1 + 5
end subroutine

subroutine sub2
  !$omp atomic seq_cst
  i2 = i2 + 5
end subroutine

subroutine sub3
  use relaxed
  !$omp atomic
  i3 = i3 + 5
end subroutine

subroutine sub4
  use relaxed
  !$omp atomic seq_cst
  i4 = i4 + 5
end subroutine

subroutine sub5
  use seq
  !$omp atomic
  i5 = i5 + 5
contains
  subroutine bar
    block
      !$omp atomic
      i5b = i5b + 5
    end block
  end
end subroutine

subroutine sub6
  use seq
  !$omp atomic seq_cst
  i6 = i6 + 5
end subroutine

subroutine sub7
  use acq
  !$omp atomic
  i7 = i7 + 5
contains
  subroutine foobar
    block
      !$omp atomic
      i7b = i7b + 5
    end block
  end
end subroutine

subroutine sub8
  use acq
  !$omp atomic seq_cst
  i8 = i8 + 5
end subroutine

! { dg-final { scan-tree-dump-times "#pragma omp atomic relaxed\[\n\r]\[^\n\r]*&i1 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i2 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic relaxed\[\n\r]\[^\n\r]*&i3 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i4 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i5 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i5 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i5b =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i6 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic release\[\n\r]\[^\n\r]*&i7 =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic release\[\n\r]\[^\n\r]*&i7b =" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp atomic seq_cst\[\n\r]\[^\n\r]*&i8 =" 1 "original" } }
