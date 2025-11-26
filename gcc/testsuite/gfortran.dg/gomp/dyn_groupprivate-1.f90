implicit none

integer :: N
N = 1024

!$omp target dyn_groupprivate(1024)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate (1024 * N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( abort ) : N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( null ) : N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target

!$omp target dyn_groupprivate ( fallback ( default_mem ) : N)  ! { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" }
!$omp end target
end
