module m
implicit none
integer :: y = 5 ! { dg-error "!.OMP GROUPPRIVATE variable 'y' at .1. must not have an initializer" }
!$omp groupprivate(y)
end

subroutine sub
  integer :: k  ! { dg-error "OpenMP groupprivate variable 'k' at .1. must have the SAVE attribute" }
  !$omp groupprivate(k)
end

subroutine sub2
  !$omp groupprivate(q)
  integer, save :: q
  !$omp groupprivate(q) ! { dg-error "Duplicate OpenMP GROUPPRIVATE attribute specified at .1." }
end
