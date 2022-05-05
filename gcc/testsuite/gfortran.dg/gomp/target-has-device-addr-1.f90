! { dg-do compile }

implicit none

integer, target :: x
integer, pointer :: ptr
integer :: a(5)

!$omp target has_device_addr(x)
!$omp end target
!$omp target has_device_addr(ptr)
!$omp end target
!$omp target has_device_addr(a)
!$omp end target
!$omp target has_device_addr(a(2:3))
!$omp end target
!$omp target has_device_addr(a(:3))
!$omp end target
!$omp target has_device_addr(a(2:))
!$omp end target
!$omp target has_device_addr(a(2))
!$omp end target

!$omp target has_device_addr(x) has_device_addr(x)  ! { dg-error "'x' present on multiple clauses" }
!$omp end target

!$omp target private(x) has_device_addr(x)  ! { dg-error "'x' present on multiple clauses" }
!$omp end target
!$omp target has_device_addr(x) private(x)  ! { dg-error "'x' present on multiple clauses" }
!$omp end target
!$omp target firstprivate(x) has_device_addr(x)  ! { dg-error "'x' present on multiple clauses" }
!$omp end target
!$omp target has_device_addr(x) firstprivate(x)  ! { dg-error "'x' present on multiple clauses" }
!$omp end target

end