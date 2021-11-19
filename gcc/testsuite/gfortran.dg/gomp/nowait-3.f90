! { dg-additional-options "-fdump-tree-original" }

subroutine foo
implicit none
integer :: i, a(5)

!$omp do
do i = 1, 5
end do
!$omp end do nowait

!$omp do simd
do i = 1, 5
end do
!$omp end do simd nowait

!$omp scope
!$omp end scope nowait

!$omp sections
  !$omp section
  block; end block
!$omp end sections nowait

!$omp single
!$omp end single nowait

!$omp target
!$omp end target nowait

!$omp target parallel
!$omp end target parallel nowait

!$omp target parallel do
do i = 1, 5
end do
!$omp end target parallel do nowait

!$omp target parallel do simd
do i = 1, 5
end do
!$omp end target parallel do simd nowait

!$omp target parallel loop
do i = 1, 5
end do
!$omp end target parallel loop nowait

!$omp target teams distribute parallel do
do i = 1, 5
end do
!$omp end target teams distribute parallel do nowait

!$omp target teams distribute parallel do simd
do i = 1, 5
end do
!$omp end target teams distribute parallel do simd nowait

!$omp target simd
do i = 1, 5
end do
!$omp end target simd nowait

!$omp target teams
!$omp end target teams nowait

!$omp target teams distribute
do i = 1, 5
end do
!$omp end target teams distribute nowait

!$omp target teams distribute simd
do i = 1, 5
end do
!$omp end target teams distribute simd nowait

!$omp target teams loop
do i = 1, 5
end do
!$omp end target teams loop nowait

!$omp workshare
A(:) = 5
!$omp end workshare nowait
end

! Note: internally, for '... parallel do ...', 'nowait' is always added
! such that for 'omp end target parallel do nowait', 'nowait' is on both
! 'target' as specified in the OpenMP spec and and on 'do' due to internal usage.

! Expected with 'nowait'

! { dg-final { scan-tree-dump-times "#pragma omp for nowait" 6 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for schedule\\(static\\) nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp sections nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp single nowait" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target nowait" 12 "original" } }

! Never:

! { dg-final { scan-tree-dump-not "#pragma omp distribute\[^\n\r]*nowait" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp loop\[^\n\r]*nowait" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp parallel\[^\n\r]*nowait" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp section\[^s\]\[^\n\r]*nowait" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp simd\[^\n\r]*nowait" "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp teams\[^\n\r]*nowait" "original" } }

! Sometimes or never with nowait:

! { dg-final { scan-tree-dump-times "#pragma omp distribute\[\n\r]" 4 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp loop\[\n\r]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel\[\n\r]" 6 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp section\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\)\[\n\r]" 5 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams\[\n\r]" 6 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp target\[\n\r]" 0 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for\[\n\r]" 0 "original" } }
