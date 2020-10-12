! { dg-do compile }
! { dg-options "-O2 -fopenmp -fdump-tree-ompexp" }
! { dg-final { scan-tree-dump-times "GOMP_loop_start " 3 "ompexp" } }
! { dg-final { scan-tree-dump-times "GOMP_loop_end_nowait " 3 "ompexp" } }

module m
  logical r
end module m

subroutine foo (a)
  use m
  implicit none
  logical a(:)
  integer :: i
  !$omp do lastprivate(conditional: r)
  do i = 1, 64
    if (a(i)) &
      r = a(i)
  end do
  !$omp end do nowait
end

subroutine bar (a)
  use m
  implicit none
  logical a(:)
  integer :: i
  !$omp do lastprivate(conditional: r) schedule (static, 4)
  do i = 1, 64
    if (a(i)) &
      r = a(i)
  end do
  !$omp end do nowait
end

subroutine baz (a)
  use m
  implicit none
  logical a(:)
  integer :: i
  !$omp do lastprivate(conditional: r) schedule (runtime)
  do i = 1, 64
    if (a(i)) &
      r = a(i)
  end do
  !$omp end do nowait
end
