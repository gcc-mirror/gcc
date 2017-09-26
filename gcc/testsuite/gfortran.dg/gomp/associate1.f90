! { dg-do compile }

program associate1
  type dl
    integer :: i
  end type
  type dt
    integer :: i
    real :: a(3, 3)
    type(dl) :: c(3, 3)
  end type
  integer :: v, i, j
  real :: a(3, 3)
  type(dt) :: b(3)
  i = 1
  j = 2
  associate(k => v, l => a(i, j), m => a(i, :)) ! { dg-warning "out of bounds" }
  associate(n => b(j)%c(:, :)%i, o => a, p => b)
!$omp parallel shared (l)	! { dg-error "ASSOCIATE name" }
!$omp end parallel
!$omp parallel firstprivate (m)	! { dg-error "ASSOCIATE name" }
!$omp end parallel
!$omp parallel reduction (+: k)	! { dg-error "ASSOCIATE name" }
!$omp end parallel
!$omp parallel do firstprivate (k)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
!$omp parallel do lastprivate (n)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
!$omp parallel do private (o)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
!$omp parallel do shared (p)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
!$omp task private (k)		! { dg-error "ASSOCIATE name" }
!$omp end task
!$omp task shared (l)		! { dg-error "ASSOCIATE name" }
!$omp end task
!$omp task firstprivate (m)	! { dg-error "ASSOCIATE name" }
!$omp end task
!$omp do private (l)		! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
!$omp do reduction (*: k)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
!$omp sections private(o)	! { dg-error "ASSOCIATE name" }
!$omp section
!$omp section
!$omp end sections
!$omp parallel sections firstprivate(p)	! { dg-error "ASSOCIATE name" }
!$omp section
!$omp section
!$omp endparallelsections
!$omp parallelsections lastprivate(m)	! { dg-error "ASSOCIATE name" }
!$omp section
!$omp section
!$omp endparallelsections
!$omp sections reduction(+:k)	! { dg-error "ASSOCIATE name" }
!$omp section
!$omp section
!$omp end sections
!$omp simd private (l)		! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
  k = 1
!$omp simd lastprivate (m)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
  k = 1
!$omp simd reduction (+: k)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10
  end do
  k = 1
!$omp simd linear (k : 2)	! { dg-error "ASSOCIATE name" }
  do i = 1, 10 ! { dg-warning "out of bounds" }
    k = k + 2
  end do
  end associate
  end associate
end program
