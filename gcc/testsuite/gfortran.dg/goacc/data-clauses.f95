! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

module test 
  implicit none
contains

  subroutine foo (vi, asa)
  integer, value :: vi
  integer :: i, ia(10)
  complex :: c, ca(10)
  real, target:: r
  real :: ra(10)
  real, pointer :: rp
  real, dimension(:), allocatable :: aa
  real, dimension(:) :: asa
  type t
    integer :: i
  end type
  type(t) :: ti
  type(t), allocatable :: tia
  type(t), target :: tit
  type(t), pointer :: tip
  rp => r
  tip => tit

  !$acc parallel deviceptr (rp) ! { dg-error "POINTER" }
  !$acc end parallel
  !$acc parallel deviceptr (vi) ! { dg-error "VALUE" }
  !$acc end parallel
  !$acc parallel deviceptr (aa) ! { dg-error "ALLOCATABLE" }
  !$acc end parallel

  !$acc parallel deviceptr (i, c, r, ia, ca, ra, asa, ti)
  !$acc end parallel
  !$acc kernels deviceptr (i, c, r, ia, ca, ra, asa, ti)
  !$acc end kernels
  !$acc serial deviceptr (i, c, r, ia, ca, ra, asa, ti)
  !$acc end serial
  !$acc data deviceptr (i, c, r, ia, ca, ra, asa, ti)
  !$acc end data


  !$acc parallel copy (tip)
  !$acc end parallel
  !$acc parallel copy (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel copyin (tip)
  !$acc end parallel
  !$acc parallel copyin (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel copyout (tip)
  !$acc end parallel
  !$acc parallel copyout (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel create (tip)
  !$acc end parallel
  !$acc parallel create (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel no_create (tip)
  !$acc end parallel
  !$acc parallel no_create (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) no_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) no_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) no_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) no_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel no_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels no_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial no_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data no_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel present (tip)
  !$acc end parallel
  !$acc parallel present (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) present (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) present (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) present (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) present (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel create (i) present (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel present (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels present (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial present (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data present (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel pcopy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc parallel pcopyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc parallel pcopyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc parallel pcreate (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel


  !$acc parallel present_or_copy (tip)
  !$acc end parallel
  !$acc parallel present_or_copy (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) present_or_copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) present_or_copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) present_or_copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) present_or_copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel create (i) present_or_copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present (i) present_or_copy (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel present_or_copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels present_or_copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial present_or_copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data present_or_copy (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel present_or_copyin (tip)
  !$acc end parallel
  !$acc parallel present_or_copyin (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel create (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present_or_copy (i) present_or_copyin (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel present_or_copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels present_or_copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial present_or_copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data present_or_copyin (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel present_or_copyout (tip)
  !$acc end parallel
  !$acc parallel present_or_copyout (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel create (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present_or_copy (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present_or_copyin (i) present_or_copyout (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel present_or_copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels present_or_copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial present_or_copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data present_or_copyout (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data


  !$acc parallel present_or_create (tip)
  !$acc end parallel
  !$acc parallel present_or_create (tia)
  !$acc end parallel
  !$acc parallel deviceptr (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copy (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyin (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel copyout (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel create (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present_or_copy (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present_or_copyin (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel
  !$acc parallel present_or_copyout (i) present_or_create (i) ! { dg-error "multiple clauses" }
  !$acc end parallel

  !$acc parallel present_or_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end parallel
  !$acc kernels present_or_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end kernels
  !$acc serial present_or_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end serial
  !$acc data present_or_create (i, c, r, ia, ca, ra, asa, rp, ti, vi, aa)
  !$acc end data

  end subroutine foo
end module test
