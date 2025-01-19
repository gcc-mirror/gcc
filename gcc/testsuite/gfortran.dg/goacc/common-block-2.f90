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

  !$acc data no_create(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end data

  !$acc parallel private(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end parallel

  !$acc parallel firstprivate(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end parallel

  !$acc serial private(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end serial

  !$acc serial firstprivate(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
  !$acc end serial

  !$acc update device(b, /blockA/, x) ! { dg-error "Symbol .x. present on multiple clauses" }
  !$acc update self(z, /blockB/, v) ! { dg-error "Symbol .z. present on multiple clauses" }
  !$acc update host(/blockA/, c) ! { dg-error "Symbol .c. present on multiple clauses" }

  !$acc enter data copyin(/blockB/, e, v, a, c, y) ! { dg-error "Symbol .y. present on multiple clauses" }
  !$acc exit data delete(/blockA/, /blockB/, e, v, a) ! { dg-error "Symbol .a. present on multiple clauses" }
end program test
