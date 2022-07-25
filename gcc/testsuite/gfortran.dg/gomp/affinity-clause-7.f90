! { dg-do compile }
! PR fortran/101330 - ICE in free_expr0(): Bad expr type
! Contributed by G.Steinmetz

  implicit none
  integer :: j, b(10)
!$omp task affinity (iterator(j=1:2:1) : b(j))
!$omp end task
!$omp task affinity (iterator(j=1:2:) : b(j)) ! { dg-error "Invalid character" }
!!$omp end task
!$omp task affinity (iterator(j=1:2:          ! { dg-error "Invalid character" }
!!$omp end task
!$omp task affinity (iterator(j=1:2:)         ! { dg-error "Invalid character" }
!!$omp end task
!$omp task affinity (iterator(j=1:2::)        ! { dg-error "Invalid character" }
!!$omp end task
!$omp task affinity (iterator(j=1:2:))        ! { dg-error "Invalid character" }
!!$omp end task
end
