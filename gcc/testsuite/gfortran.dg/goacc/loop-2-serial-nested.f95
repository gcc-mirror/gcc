program test
  implicit none
  integer :: i, j

  !$acc serial loop gang
  DO i = 1,10
    !$acc serial loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc serial loop worker
  DO i = 1,10
    !$acc serial loop worker ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc serial loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc serial loop vector
  DO i = 1,10
    !$acc serial loop vector ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc serial loop worker ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc serial loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO
end
