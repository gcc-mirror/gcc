! { dg-additional-options "-fdump-tree-original" }

subroutine bar
integer, target :: x, x2
integer, allocatable, target :: y(:,:), z(:,:)
x = 7
!$omp target enter data map(to:x, x2)

x = 8
!$omp target data map(always, to: x)
call foo(x)
!$omp end target data

!$omp target data use_device_ptr(x)
call foo2(x)
!$omp end target data

!$omp target data use_device_addr(x2)
call foo2(x)
!$omp end target data
!$omp target exit data map(release:x)

!$omp target data map(y) use_device_addr(y)
call foo3(y)
!$omp end target data

!$omp target data map(z) use_device_ptr(z)
call foo3(z)
!$omp end target data
end 

! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:x\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,to:x\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data use_device_addr\\(x\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data use_device_addr\\(x2\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(release:x\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:\\*\\(c_char \\*\\) y.data \\\[len: .*\\) map\\(to:y \\\[pointer set, len: .*\\) map\\(alloc:.*y.data \\\[pointer assign, bias: 0\\\]\\) use_device_addr\\(y\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:\\*\\(c_char \\*\\) z.data \\\[len: .*\\) map\\(to:z \\\[pointer set, len: .*\\) map\\(alloc:.*z.data \\\[pointer assign, bias: 0\\\]\\) use_device_addr\\(z\\)" 1 "original" } }
