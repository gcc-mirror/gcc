! { dg-do run }
! { dg-additional-options "-fopenmp" }
!
! PR fortran/120286 - scalar class pointers in OpenMP private/firstprivate
! clauses must preserve association status without taking ownership.

program main
  implicit none

  type foo_t
    integer :: dummy
  end type foo_t

  type fooPtr_t
    class(foo_t), pointer :: p
  end type fooPtr_t

  type fooPtrStack_t
    class(fooPtr_t), allocatable :: list(:)
  end type fooPtrStack_t

  type(fooPtrStack_t) :: x
  class(foo_t), pointer :: ptr
  integer :: it, n
  logical :: ok

  allocate (x%list(1))
  allocate (x%list(1)%p)
  x%list(1)%p%dummy = 7

  do it = 1, 16
!$omp parallel do default(none) num_threads(2) private(n, ptr) shared(x)
    do n = 1, 1
      ptr => x%list(n)%p
    end do
!$omp end parallel do
  end do

  if (.not. associated (x%list(1)%p)) stop 1
  if (x%list(1)%p%dummy /= 7) stop 2

  ptr => x%list(1)%p
  ok = .false.

!$omp parallel default(none) num_threads(1) firstprivate(ptr) shared(x, ok)
  ok = associated (ptr, x%list(1)%p) .and. ptr%dummy == 7
!$omp end parallel

  if (.not. ok) stop 3
end program main
