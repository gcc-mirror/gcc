! PR fortran/69128
! { dg-do compile }

program test
  implicit none
  interface
    subroutine use(b, c)
      real, allocatable :: b(:), c(:)
    end subroutine
  end interface
  real, allocatable :: a(:,:), b(:), c(:)
  integer :: dim1, dim2, i,j
  dim1=10000
  dim2=500
  allocate(a(dim1,dim2),b(dim1),c(dim1))
  call random_number(a)

!$omp parallel workshare
  b(:) = maxval(a(:,:), dim=2)
  c(:) = sum(a(:,:), dim=2)
!$omp end parallel workshare
  call use(b, c)
end program
