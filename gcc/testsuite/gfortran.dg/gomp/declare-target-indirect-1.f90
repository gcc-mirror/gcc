! { dg-do compile }
! { dg-options "-fopenmp" }

module m
  integer :: a
  integer, parameter :: X = 1
  integer, parameter :: Y = 2

  ! Indirect on a variable should have no effect.
  integer :: z
  !$omp declare target to (z) indirect
contains
  subroutine sub1
    !$omp declare target indirect to (sub1)
  end subroutine

  subroutine sub2
    !$omp declare target enter (sub2) indirect (.true.)
  end subroutine

  subroutine sub3
    !$omp declare target to (sub3) indirect (.false.)
  end subroutine

  subroutine sub4
    !$omp declare target to (sub4) indirect (1) ! { dg-error "INDIRECT clause at .1. requires a constant logical expression" }
  end subroutine

  ! Compile-time non-constant expressions are not allowed.
  subroutine sub5
    !$omp declare target indirect (a > 0) to (sub5) ! { dg-error "INDIRECT clause at .1. requires a constant logical expression" }
  end subroutine

  ! Compile-time constant expressions are permissible.
  subroutine sub6
    !$omp declare target indirect (X .eq. Y) to (sub6)
  end subroutine

  subroutine sub7
    !$omp declare target indirect ! { dg-warning "OMP DECLARE TARGET directive at .1. with only DEVICE_TYPE or INDIRECT clauses is ignored" }
  end subroutine

  subroutine sub8
    !$omp declare target indirect (.true.) indirect (.false.) to (sub8) ! { dg-error "Duplicated .indirect. clause at .1." }
  end subroutine

  subroutine sub9
    !$omp declare target to (sub9) indirect ("abs") ! { dg-error "INDIRECT clause at .1. requires a constant logical expression" }
  end subroutine

  subroutine sub10
    !$omp declare target to (sub10) indirect (5.5) ! { dg-error "INDIRECT clause at .1. requires a constant logical expression" }
  end subroutine

  subroutine sub11
    !$omp declare target indirect (.true.) device_type (host) enter (sub11) ! { dg-error "DEVICE_TYPE must be ANY when used with INDIRECT at .1." }
  end subroutine

  subroutine sub12
    !$omp declare target indirect (.false.) device_type (nohost) enter (sub12)
  end subroutine
end module
