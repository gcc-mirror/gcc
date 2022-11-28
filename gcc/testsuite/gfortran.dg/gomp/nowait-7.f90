! { dg-additional-options "-fdump-tree-original" }

subroutine foo
implicit none
integer :: i, a(5)

!$omp do nowait
do i = 1, 5
end do
!$omp end do

!$omp do simd nowait
do i = 1, 5
end do
!$omp end do simd

!$omp scope nowait
!$omp end scope

!$omp sections nowait
  !$omp section
  block; end block
!$omp end sections

!$omp single nowait
!$omp end single

!$omp target nowait
!$omp end target

!$omp target parallel nowait
!$omp end target parallel

!$omp target parallel do nowait
do i = 1, 5
end do
!$omp end target parallel do

!$omp target parallel do simd nowait
do i = 1, 5
end do
!$omp end target parallel do simd

!$omp target parallel loop nowait
do i = 1, 5
end do
!$omp end target parallel loop

!$omp target teams distribute parallel do nowait
do i = 1, 5
end do
!$omp end target teams distribute parallel do

!$omp target teams distribute parallel do simd nowait
do i = 1, 5
end do
!$omp end target teams distribute parallel do simd

!$omp target simd nowait
do i = 1, 5
end do
!$omp end target simd

!$omp target teams nowait
!$omp end target teams

!$omp target teams distribute nowait
do i = 1, 5
end do
!$omp end target teams distribute

!$omp target teams distribute simd nowait
do i = 1, 5
end do
!$omp end target teams distribute simd

!$omp target teams loop nowait
do i = 1, 5
end do
!$omp end target teams loop

!$omp workshare nowait
A(:) = 5
!$omp end workshare
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
