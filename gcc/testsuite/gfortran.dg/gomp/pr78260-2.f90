! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-original" }

! PR fortran/78260

module m
  implicit none
  integer :: n = 0
contains
  integer function f1()
    !$omp target data map(f1)
    !$omp target update to(f1)
    f1 = 5 
    !$omp end target data
  end function f1

  integer function f2()
    dimension :: f2(1)
    !$omp target data map(f2)
    !$omp target update to(f2)
    f2(1) = 5 
    !$omp end target data
  end function f2

  integer function f3() result(res)
    dimension :: res(1)
    !$omp target data map(res)
    !$omp target update to(res)
    res(1) = 5 
    !$omp end target data
  end function f3

  integer function f4() result(res)
    allocatable :: res
    dimension :: res(:)
    !$omp target data map(res)
    !$omp target update to(res)
    res = [5]
    !$omp end target data
  end function f4

  subroutine sub()
    integer, allocatable :: arr(:)
    !$omp target data map(arr)
    !$omp target update to(arr)
    arr = [5]
    !$omp end target data
  end subroutine sub
end module m

! Check for multiplication: len = arrays_size * 4<bytes>:
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = D\\.\[0-9\]+ \\* 4;" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) arr.data \\\[len: D.\[0-9\]+\\\]\\) map\\(to:arr \\\[pointer set, len: ..\\\]\\) map\\(alloc:\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) arr.data \\\[pointer assign, bias: 0\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target update to\\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) arr.data \\\[len: D.\[0-9\]+ \\* 4\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) __result->data \\\[len: D.\[0-9\]+\\\]\\) map\\(to:\\*__result \\\[pointer set, len: ..\\\]\\) map\\(alloc:\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) __result->data \\\[pointer assign, bias: 0\\\]\\) map\\(alloc:__result \\\[pointer assign, bias: 0\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target update to\\(\\*\\(integer\\(kind=4\\)\\\[0:\\\] \\* restrict\\) __result->data \\\[len: D.\[0-9\]+ \\* 4\\\]\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:\\*__result.0\\) map\\(alloc:__result.0 \\\[pointer assign, bias: 0\\\]\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target update to\\(\\*__result.0\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:__result_f1\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target update to\\(__result_f1\\)" 1 "original" } }

