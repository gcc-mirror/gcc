program test
  implicit none
  integer :: i, j

  !$acc parallel loop gang
  DO i = 1,10
    !$acc parallel loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc parallel loop worker
  DO i = 1,10
    !$acc parallel loop worker ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc parallel loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc parallel loop vector
  DO i = 1,10
    !$acc parallel loop vector ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc parallel loop worker ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc parallel loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO
end
