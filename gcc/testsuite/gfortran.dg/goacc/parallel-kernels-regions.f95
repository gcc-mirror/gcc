! { dg-do compile } 

! OpenACC 2.0 allows nested parallel/kernels regions, but this is not yet
! supported.

program test
  implicit none

  integer :: i

  !$acc parallel
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
  !$acc end parallel

  !$acc parallel
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
  !$acc end parallel

  !$acc parallel
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
  !$acc end parallel

  !$acc kernels
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
  !$acc end kernels

  !$acc kernels
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
  !$acc end kernels

  !$acc kernels
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
  !$acc end kernels

  !$acc parallel
    !$acc data ! { dg-error ".data. construct inside of .parallel. region" }
    !$acc end data
  !$acc end parallel

  !$acc kernels
    !$acc data ! { dg-error ".data. construct inside of .kernels. region" }
    !$acc end data
  !$acc end kernels
  
end program test
