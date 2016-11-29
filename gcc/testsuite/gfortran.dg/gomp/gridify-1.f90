! { dg-do compile }
! { dg-require-effective-target offload_hsa }
! { dg-options "-fopenmp -fdump-tree-omplower-details" } */

subroutine vector_square(n, a, b)
      integer i, n, b(n), a(n)
!$omp target teams
!$omp distribute parallel do
      do i=1,n
          b(i) = a(i) * a(i)
      enddo
!$omp end distribute parallel do
!$omp end target teams
end subroutine vector_square

! { dg-final { scan-tree-dump "Target construct will be turned into a gridified HSA kernel" "omplower" } }
