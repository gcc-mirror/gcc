! Test to ensure that IS_DEVICE_PTR is removed for non-used variables.

! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program main
  use iso_c_binding
  implicit none

  integer :: x, y
  call foo (x, y)

contains
  subroutine foo (a, b)
    integer, target :: a, b

    !$omp target data map(a, b) use_device_ptr(a, b)
      !$omp target is_device_ptr(a, b)
        a = 42
      !$omp end target
    !$omp end target data
  end subroutine foo

end program main

! { dg-final { scan-tree-dump "has_device_addr\\(a\\)"  "gimple" } }
! { dg-final { scan-tree-dump-not "has_device_addr\\(b\\)"  "gimple" } }
! { dg-final { scan-tree-dump-not "is_device_ptr\\(b\\)"  "gimple" } }
