! PR fortran/44536
! { dg-do compile }
! { dg-options "-fopenmp" }
      subroutine foo (a, i, j)
        integer, dimension(:) :: a
        integer :: i, j
!$omp parallel default(none) shared(i, j)	! { dg-message "note: enclosing 'parallel'" }
        j=a(i)					! { dg-error "not specified in" }
!$omp end parallel
      end subroutine
