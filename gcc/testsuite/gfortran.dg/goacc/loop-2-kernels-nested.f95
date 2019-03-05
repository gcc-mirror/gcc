program test
  implicit none
  integer :: i, j

  !$acc kernels loop gang
  DO i = 1,10
    !$acc kernels loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc kernels loop worker
  DO i = 1,10
    !$acc kernels loop worker ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc kernels loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc kernels loop vector
  DO i = 1,10
    !$acc kernels loop vector ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc kernels loop worker ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
    !$acc kernels loop gang ! { dg-bogus "OpenACC construct inside of non-OpenACC region" "TODO" { xfail *-*-* } }
    DO j = 1,10
    ENDDO
  ENDDO
end
