! { dg-do compile } 

! The OpenACC specification allows nested compute constructs, but we don't
! support that yet.  */

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
    !$acc serial ! { dg-bogus ".serial. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end serial
  !$acc end parallel

  !$acc parallel
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
    !$acc serial ! { dg-bogus ".serial. construct inside of .parallel. region" "not implemented" { xfail *-*-* } }
    !$acc end serial
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
    !$acc serial ! { dg-bogus ".serial. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end serial
  !$acc end kernels

  !$acc kernels
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
    !$acc serial ! { dg-bogus ".serial. construct inside of .kernels. region" "not implemented" { xfail *-*-* } }
    !$acc end serial
  !$acc end kernels


  !$acc serial
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .serial. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
  !$acc end serial

  !$acc serial
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .serial. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
  !$acc end serial

  !$acc serial
    !$acc serial ! { dg-bogus ".serial. construct inside of .serial. region" "not implemented" { xfail *-*-* } }
    !$acc end serial
  !$acc end serial

  !$acc serial
    !$acc parallel ! { dg-bogus ".parallel. construct inside of .serial. region" "not implemented" { xfail *-*-* } }
    !$acc end parallel
    !$acc kernels ! { dg-bogus ".kernels. construct inside of .serial. region" "not implemented" { xfail *-*-* } }
    !$acc end kernels
    !$acc serial ! { dg-bogus ".serial. construct inside of .serial. region" "not implemented" { xfail *-*-* } }
    !$acc end serial
  !$acc end serial


  !$acc parallel
    !$acc data ! { dg-error ".data. construct inside of .parallel. region" }
    !$acc end data
  !$acc end parallel

  !$acc kernels
    !$acc data ! { dg-error ".data. construct inside of .kernels. region" }
    !$acc end data
  !$acc end kernels
  
  !$acc serial
    !$acc data ! { dg-error ".data. construct inside of .serial. region" }
    !$acc end data
  !$acc end serial

end program test
