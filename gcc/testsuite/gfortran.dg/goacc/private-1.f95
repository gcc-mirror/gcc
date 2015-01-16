! { dg-do compile }
! { dg-additional-options "-fdump-tree-omplower" }

! test for implicit private clauses in do loops

program test
  implicit none
  integer :: i, j, k

  !$acc parallel
  !$acc loop
  do i = 1, 100
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop
  do i = 1, 100
     do j = 1, 100
     end do
  end do
  !$acc end parallel

  !$acc parallel
  !$acc loop
  do i = 1, 100
     do j = 1, 100
        do k = 1, 100
        end do
     end do
  end do
  !$acc end parallel
end program test
! { dg-final { scan-tree-dump-times "pragma omp target oacc_parallel" 3 "omplower" } }
! { dg-final { scan-tree-dump-times "private\\(i\\)" 3 "omplower" } }
! { dg-final { scan-tree-dump-times "private\\(j\\)" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "private\\(k\\)" 1 "omplower" } }
