! { dg-do compile }
subroutine test(b,c,d)
  implicit none
  integer, value, target :: b
  integer, pointer :: c
  integer, allocatable, target :: d

  integer, target :: a(5)

  !$omp target is_device_ptr(a) ! Valid since OpenMP 5.1
  !$omp end target

  !$omp target is_device_ptr(b) ! Valid since OpenMP 5.1
  !$omp end target

  !$omp target is_device_ptr(c) ! Valid since OpenMP 5.1
  !$omp end target

  !$omp target is_device_ptr(d) ! Valid since OpenMP 5.1
  !$omp end target

  !$omp target data map(a) use_device_addr(a)  ! Should be okay
  !$omp end target data

  !$omp target data map(c) use_device_ptr(c)  ! Should be okay
  !$omp end target data
end subroutine test
