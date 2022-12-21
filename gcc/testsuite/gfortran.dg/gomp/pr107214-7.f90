! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

integer :: x

!$omp target simd map(x) private(x)
do i=1,1
  x = x + 1
end do
!$omp end target simd

!$omp target teams distribute simd map(x) private(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams distribute simd

!$omp target parallel do simd map(x) private(x)
do i=1,1
  x = x + 1
end do
!$omp end target parallel do simd

!$omp target teams distribute parallel do simd map(x) private(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams distribute parallel do simd

! { dg-final { scan-tree-dump-times {omp target map\(tofrom:x\)} 4 "original" } }
! { dg-final { scan-tree-dump-times {(?n)omp simd.* private\(x\)} 4 "original" } }

end
