! PR middle-end/113436
! { dg-do compile }
! { dg-options "-fopenmp -fno-automatic -fdump-tree-omplower" }

program g
  use omp_lib
  implicit none

  integer :: A(10)
  integer :: i

  do i = 1, 10
    A(i) = i + 5
  end do
      
  !$omp target firstprivate(A) allocate(allocator(omp_high_bw_mem_alloc), align(32): A)
      do i = 1, 10
        A(i) = -i + 23
      end do
  !$omp end target
end program g

! { dg-excess-errors "Flag '-fno-automatic' overwrites '-frecursive' implied by '-fopenmp'" }
! { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(32, 40, 4\\\);" "omplower" { target int32 } } }
! { dg-final { scan-tree-dump "\\\(\\\*D\\\.\[0-9\]\+\\\) = \\\(\\\*D\\\.\[0-9\]\+\\\);" "omplower" } }
! { dg-final { scan-tree-dump "\\\(\\\*D\\\.\[0-9\]\+\\\)\\\[D\\\.\[0-9\]\+\\\] = D\\\.\[0-9\]\+;" "omplower" } }
! { dg-final { scan-tree-dump "__builtin_GOMP_free \\\(D\\\.\[0-9\]+, 4\\\)" "omplower" } }
