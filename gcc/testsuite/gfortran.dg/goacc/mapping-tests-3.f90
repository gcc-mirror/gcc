! { dg-additional-options "-fdump-tree-gimple" }

subroutine foo
  type one
    integer i, j
  end type
  type two
    type(one) A, B
  end type

  type(two) x

  !$acc enter data copyin(x%A)
! { dg-final { scan-tree-dump-times "omp target oacc_enter_exit_data map\\(struct:x \\\[len: 1\\\]\\) map\\(to:x.a \\\[len: \[0-9\]+\\\]\\)" 1 "gimple" } }
end
