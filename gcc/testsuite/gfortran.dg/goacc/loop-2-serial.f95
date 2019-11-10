! See also "../../c-c++-common/goacc/loop-2-serial.c".

program test
  implicit none
  integer :: i, j

  !$acc serial
    !$acc loop auto
    DO i = 1,10
    ENDDO
    !$acc loop gang
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:5)
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:*)
    DO i = 1,10
    ENDDO
    !$acc loop gang
    DO i = 1,10
      !$acc loop vector 
      DO j = 1,10
      ENDDO
      !$acc loop worker 
      DO j = 1,10
      ENDDO
      !$acc loop gang ! { dg-error "inner loop uses same OpenACC parallelism as containing loop" }
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop seq gang ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO

    !$acc loop worker
    DO i = 1,10
    ENDDO
    !$acc loop worker
    DO i = 1,10
      !$acc loop vector 
      DO j = 1,10
      ENDDO
      !$acc loop worker ! { dg-error "inner loop uses same OpenACC parallelism as containing loop" }
      DO j = 1,10
      ENDDO
      !$acc loop gang ! { dg-error "incorrectly nested OpenACC loop parallelism" }
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop seq worker ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO
    !$acc loop gang worker
    DO i = 1,10
    ENDDO

    !$acc loop vector
    DO i = 1,10
    ENDDO
    !$acc loop vector
    DO i = 1,10
      !$acc loop vector ! { dg-error "inner loop uses same OpenACC parallelism as containing loop" }
      DO j = 1,10
      ENDDO
      !$acc loop worker ! { dg-error "incorrectly nested OpenACC loop parallelism" }
      DO j = 1,10
      ENDDO
      !$acc loop gang ! { dg-error "incorrectly nested OpenACC loop parallelism" }
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop seq vector ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO
    !$acc loop gang vector
    DO i = 1,10
    ENDDO
    !$acc loop worker vector
    DO i = 1,10
    ENDDO

    !$acc loop auto
    DO i = 1,10
    ENDDO
    !$acc loop seq auto ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO
    !$acc loop gang auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO
    !$acc loop worker auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO
    !$acc loop vector auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
    DO i = 1,10
    ENDDO
  !$acc end serial

  !$acc serial loop auto
  DO i = 1,10
  ENDDO
  !$acc serial loop gang
  DO i = 1,10
  ENDDO
  !$acc serial loop gang(static:5)
  DO i = 1,10
  ENDDO
  !$acc serial loop gang(static:*)
  DO i = 1,10
  ENDDO
  !$acc serial loop seq gang ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO

  !$acc serial loop worker
  DO i = 1,10
  ENDDO
  !$acc serial loop seq worker ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc serial loop gang worker
  DO i = 1,10
  ENDDO

  !$acc serial loop vector
  DO i = 1,10
  ENDDO
  !$acc serial loop seq vector ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc serial loop gang vector
  DO i = 1,10
  ENDDO
  !$acc serial loop worker vector
  DO i = 1,10
  ENDDO

  !$acc serial loop auto
  DO i = 1,10
  ENDDO
  !$acc serial loop seq auto ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc serial loop gang auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc serial loop worker auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc serial loop vector auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
end
