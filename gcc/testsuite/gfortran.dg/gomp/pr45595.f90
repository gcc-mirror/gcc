! PR fortran/45595
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo(l,u)
  integer :: k,l,u
  !$omp parallel do shared(l,u) collapse(3)	! { dg-error "not enough DO loops" }
    do k = l,u
    end do
end subroutine
