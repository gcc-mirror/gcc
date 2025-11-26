module m
implicit none
integer :: y = 5 ! { dg-error "!.OMP GROUPPRIVATE variable 'y' at .1. must not have an initializer" }
common /b_y/ y
!$omp groupprivate(/b_y/)
end

subroutine sub
  integer, save :: k
  common /b_k/ k  ! { dg-error "COMMON attribute conflicts with SAVE attribute in 'k' at .1." }
  !$omp groupprivate(/b_k/)  ! { dg-error "COMMON attribute conflicts with SAVE attribute in 'k' at .1." }
end

subroutine sub2
  common /b_q/ q
  !$omp groupprivate(/b_q/)
  integer :: q
  !$omp groupprivate(/b_q/) ! { dg-error "Duplicate OpenMP GROUPPRIVATE attribute specified at .1." }
end

subroutine dupl
  integer :: a,b,c,d
  integer :: u,v,w,x
  common /b_a/ a
  common /b_b/ b
  common /b_c/ c
  common /b_d/ d

  !$omp groupprivate(/b_a/,u,/b_a/) ! { dg-error "Duplicate OpenMP GROUPPRIVATE attribute specified" }
  !$omp groupprivate(v,/b_b/,v) ! { dg-error "Duplicate OpenMP GROUPPRIVATE attribute specified" }

  !$omp threadprivate(/b_a/,u,/b_a/) ! { dg-error "Duplicate THREADPRIVATE attribute specified" }
  !$omp threadprivate(v,/b_b/,v) ! { dg-error "Duplicate THREADPRIVATE attribute specified" }
end
