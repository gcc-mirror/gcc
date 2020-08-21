! Fortran version of PR c/67500
! { dg-do compile }

subroutine f1
  !$omp declare simd simdlen(d)   ! { dg-error "requires a scalar INTEGER expression" }
end subroutine

subroutine f2
  !$omp declare simd simdlen(0.5)  ! { dg-error "requires a scalar INTEGER expression" }
end

subroutine f3 (i)
  !$omp declare simd simdlen(-2)   ! { dg-warning "INTEGER expression of SIMDLEN clause at .1. must be positive" }
end subroutine

subroutine f4
  !$omp declare simd simdlen(0)	   ! { dg-warning "INTEGER expression of SIMDLEN clause at .1. must be positive" }
end

subroutine foo(p, d, n)
  integer, allocatable :: p(:)
  real, value :: d
  integer, value :: n
  integer :: i

  !$omp simd safelen(d)     ! { dg-error "requires a scalar INTEGER expression" }
  do i = 1, 16
  end do

  !$omp simd safelen(0.5)   ! { dg-error "requires a scalar INTEGER expression" }
  do i = 1, 16
  end do

  !$omp simd safelen(-2)    ! { dg-warning "INTEGER expression of SAFELEN clause at .1. must be positive" }
  do i = 1, 16
  end do

  !$omp simd safelen(0)     ! { dg-warning "INTEGER expression of SAFELEN clause at .1. must be positive" }
  do i = 1, 16
  end do

  !$omp simd aligned(p:n)   ! { dg-error "requires a scalar positive constant integer alignment expression" }
  do i = 1, 16
  end do

  !$omp simd aligned(p:0.5)  ! { dg-error "requires a scalar positive constant integer alignment expression" }
  do i = 1, 16
  end do

  !$omp simd aligned(p:-2)  ! { dg-error "requires a scalar positive constant integer alignment expression" }
  do i = 1, 16
  end do

  !$omp simd aligned(p:0)    ! { dg-error "requires a scalar positive constant integer alignment expression" }
  do i = 1, 16
  end do
end
