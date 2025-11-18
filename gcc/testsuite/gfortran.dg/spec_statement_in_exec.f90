! { dg-do compile }
! { dg-options "-fopenmp" }
! Test improved error messages for specification statements in executable section
! PR fortran/32365 - Better error message for specification statement in executable section

subroutine test_spec_in_exec
  implicit none
  integer :: i

  ! First executable statement
  i = 1

  ! Test key specification statement types
  integer :: j                     ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
  real :: x                       ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
  complex :: z                    ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
  logical :: flag                  ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
  character(len=20) :: name       ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
  double precision :: d           ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
  common /myblock/ i              ! { dg-error "COMMON statement at \\(1\\) cannot appear after executable statements" }
  equivalence (i, i)              ! { dg-error "EQUIVALENCE statement at \\(1\\) cannot appear after executable statements" }
  namelist /nml/ i                ! { dg-error "NAMELIST statement at \\(1\\) cannot appear after executable statements" }
!$omp threadprivate(i)             ! { dg-error "THREADPRIVATE statement at \\(1\\) cannot appear after executable statements" }
!$omp declare target (i)           ! { dg-error "DECLARE TARGET statement at \\(1\\) cannot appear after executable statements" }

end subroutine test_spec_in_exec
