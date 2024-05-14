! { dg-do compile }
! { dg-additional-options "-foffload=disable -fdump-tree-original -fdump-tree-gimple" }

program main
implicit none

integer, parameter :: N = 10
double precision, parameter :: S = 2.0
double precision :: a(N)

call init (N, a)
call f1 (N, a, S)
call check (N, a, S)

call init (N, a)
call f2 (N, a, S)
call check (N, a, S)

call init (N, a)
call f3 (N, a, S)
call check (N, a, S)

call init (N, a)
call f4 (N, a, S)
call check (N, a, S)

call init (N, a)
call f5 (N, a, S)
call check (N, a, S)

call init (N, a)
call f6 (N, a, S)
call check (N, a, S)

call init (N, a)
call f7 (N, a, S)
call check (N, a, S)

call init (N, a)
call f8 (N, a, S)
call check (N, a, S)

call init (N, a)
call f9 (N, a, S)
call check (N, a, S)

contains

subroutine init (n, a)
  implicit none
  integer :: n
  double precision :: a(n)
  integer :: i
  do i = 1, n
    a(i) = i
  end do
end subroutine

subroutine check (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
  do i = 1, n
    if (a(i) /= i * s) error stop
  end do
end subroutine

! Check various combinations for enforcing correct ordering of 
! construct matches.
subroutine f1 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target teams
!$omp parallel  
!$omp metadirective &
!$omp &  when (construct={target} &
!$omp &	: do) &
!$omp &  default (error at(execution) message("f1 match failed"))
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end parallel  
!$omp end target teams
end subroutine

subroutine f2 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target teams
!$omp parallel
!$omp metadirective &
!$omp &  when (construct={teams, parallel} &
!$omp &	: do) &
!$omp &  default (error at(execution) message("f2 match failed"))
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end parallel  
!$omp end target teams
end subroutine

subroutine f3 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target teams
!$omp parallel
!$omp metadirective &
!$omp &  when (construct={target, teams, parallel} &
!$omp &	: do) &
!$omp &  default (error at(execution) message("f3 match failed"))
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end parallel  
!$omp end target teams
end subroutine

subroutine f4 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target teams
!$omp parallel
!$omp metadirective &
!$omp &  when (construct={target, parallel} &
!$omp &	: do) &
!$omp &  default (error at(execution) message("f4 match failed"))
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end parallel  
!$omp end target teams
end subroutine

subroutine f5 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target teams
!$omp parallel
!$omp metadirective &
!$omp &  when (construct={target, teams} &
!$omp &	: do) &
!$omp &  default (error at(execution) message("f5 match failed"))
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end parallel  
!$omp end target teams
end subroutine

! Next batch is for things where the construct doesn't match the context.
subroutine f6 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target
!$omp teams
!$omp metadirective &
!$omp &  when (construct={parallel} &
!$omp &	: error at(execution) message("f6 match failed")) &
!$omp &  default (parallel do)
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end teams
!$omp end target
end subroutine

subroutine f7 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target
!$omp teams
!$omp metadirective &
!$omp &  when (construct={target, parallel} &
!$omp &	: error at(execution) message("f7 match failed")) &
!$omp &  default (parallel do)
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end teams
!$omp end target
end subroutine

subroutine f8 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target
!$omp teams
!$omp metadirective &
!$omp &  when (construct={parallel, target} &
!$omp &	: error at(execution) message("f8 match failed")) &
!$omp &  default (parallel do)
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end teams
!$omp end target
end subroutine

! Next test choosing the best alternative when there are multiple
! matches.
subroutine f9 (n, a, s)
  implicit none
  integer :: n
  double precision :: a(n)
  double precision :: s
  integer :: i
!$omp target teams
!$omp parallel
!$omp metadirective &
!$omp &  when (construct={teams, parallel} &
!$omp &	: error at(execution) message("f9 match incorrect 1")) &
!$omp &  when (construct={target, teams, parallel} &
!$omp &	: do) &
!$omp &  when (construct={target, teams} &
!$omp &	: error at(execution) message("f9 match incorrect 2")) &
!$omp &  default (error at(execution) message("f9 match failed"))
  do i = 1, n
    a(i) = a(i) * s
  end do
!$omp end parallel  
!$omp end target teams
end subroutine

end program

! Note there are no tests for the matching the extended simd clause
! syntax, which is only useful for "declare variant".


! After parsing, there should be a runtime error call for each of the
! failure cases, but they should all be optimized away during OMP 
! lowering.
! { dg-final { scan-tree-dump-times "__builtin_GOMP_error" 11 "original" } }
! { dg-final { scan-tree-dump-not "__builtin_GOMP_error" "gimple" } }
