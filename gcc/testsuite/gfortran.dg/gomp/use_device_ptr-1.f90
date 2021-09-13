! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! PR fortran/98476

use iso_c_binding, only: c_ptr
implicit none (external, type)

interface
  subroutine bar(x)
    import
    type(c_ptr), value :: x
  end
end interface

type(c_ptr) :: x

!$omp target data map(alloc: x)
!$omp target data use_device_ptr(x)
  call bar(x)
!$omp end target data
!$omp end target data
end

! { dg-final { scan-tree-dump-times "pragma omp target data use_device_ptr\\(x\\)" 1 "original" } }
