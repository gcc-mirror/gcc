integer function s1 (a1, a2, a3) result(r)
  implicit none
  integer :: a1, a2, a3
  integer :: i, j, k
  procedure(integer) :: iii
  
  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        !$omp scan exclusive (r)  ! { dg-warning "!.OMP SCAN at .1. with zero executable statements in preceding structured block sequence" }
        call f1 (2, k, r)
      end do
    end do 
  end do

  !$omp simd collapse(3) reduction (inscan, +:r)
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        r = r + iii (i, j, k)
        !$omp scan exclusive (r)  ! { dg-warning "!.OMP SCAN at .1. with zero executable statements in succeeding structured block sequence" }
      end do
    end do 
  end do

  !$omp simd collapse(3) reduction (inscan, +:r)
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        !$omp scan inclusive (r)
           ! { dg-warning "!.OMP SCAN at .1. with zero executable statements in preceding structured block sequence" "" { target *-*-* } .-1 }
           ! { dg-warning "!.OMP SCAN at .1. with zero executable statements in succeeding structured block sequence" "" { target *-*-* } .-2 }
      end do
    end do 
  end do
end function

integer function s2 (a1, a2, a3) result(r)
  implicit none
  integer :: a1, a2, a3
  integer :: i, j, k
  procedure(integer) :: iii
  
  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)  ! { dg-error "With INSCAN at .1., expected loop body with !.OMP SCAN between two structured block sequences" }
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        call f1 (2, k, r)
        r = r + iii (i, j, k)
      end do
    end do 
  end do

  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)  ! { dg-error "With INSCAN at .1., expected loop body with !.OMP SCAN between two structured block sequences" }
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
      end do
    end do 
  end do

  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        call f1 (2, k, r)
        !$omp scan inclusive (r)
        !$omp scan inclusive (r)  ! { dg-error "Unexpected !.OMP SCAN at .1. outside loop construct with 'inscan' REDUCTION clause" }
        r = r + iii (i, j, k)
      end do
    end do 
  end do

  !$omp scan inclusive (r)  ! { dg-error "Unexpected !.OMP SCAN at .1. outside loop construct with 'inscan' REDUCTION clause" }

  r = 0
  !$omp simd collapse(3) reduction (inscan, +:r)  ! { dg-error "With INSCAN at .1., expected loop body with !.OMP SCAN between two structured block sequences" }
  do i = 1, a1
    do j = 1, a2
      do k = 1, a3
        call f1 (2, k, r)
        block
          !$omp scan inclusive (r)  ! { dg-error "Unexpected !.OMP SCAN at .1. outside loop construct with 'inscan' REDUCTION clause" }
        end block
        r = r + iii (i, j, k)
      end do
    end do 
  end do


end function
