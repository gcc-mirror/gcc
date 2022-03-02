! Test to ensure that HAS_DEVICE_ADDR is removed for non-used variables.

! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program main
  use iso_c_binding
  implicit none

  integer :: x, y
  call foo (x, y)

contains
  subroutine foo (a, b)
    integer :: a, b

    !$omp target data map(a) use_device_addr(a)
      !$omp target has_device_addr(a)
        a = 42
      !$omp end target
    !$omp end target data
  end subroutine foo

end program main

! { dg-final { scan-tree-dump "has_device_addr\\(a\\)"  "gimple" } }
! { dg-final { scan-tree-dump-not "has_device_addr\\(b\\)"  "gimple" } }
