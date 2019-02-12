! Test data clauses involving common blocks and common block data.
! Specifically, resolver errors such as duplicate data clauses.

program test
  implicit none
  integer, parameter :: n = 10
  integer a(n), b(n), c, d(n), e
  real*4 x(n), y(n), z, w(n), v
  common /blockA/ a, c, x
  common /blockB/ b, y, z

  !$acc data copy(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data copyin(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data copyout(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data create(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data copyout(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data pcopy(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data pcopyin(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data pcopyout(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data pcreate(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc data pcopyout(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc parallel private(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end parallel

  !$acc parallel firstprivate(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end parallel

  !$acc exit data delete(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
end program test
