! See also "../../c-c++-common/goacc/loop-2-kernels.c".

program test
  implicit none
  integer :: i, j

  !$acc kernels
    !$acc loop auto
    DO i = 1,10
    ENDDO
    !$acc loop gang
    DO i = 1,10
    ENDDO
    !$acc loop gang(5)
    DO i = 1,10
    ENDDO
    !$acc loop gang(num:5)
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
    !$acc loop worker(5)
    DO i = 1,10
    ENDDO
    !$acc loop worker(num:5)
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
      !$acc loop gang ! { dg-error "" "TODO" { xfail *-*-* } }
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
    !$acc loop vector(5)
    DO i = 1,10
    ENDDO
    !$acc loop vector(length:5)
    DO i = 1,10
    ENDDO
    !$acc loop vector
    DO i = 1,10
      !$acc loop vector ! { dg-error "inner loop uses same OpenACC parallelism as containing loop" }
      DO j = 1,10
      ENDDO
      !$acc loop worker ! { dg-error "" "TODO" { xfail *-*-* } }
      DO j = 1,10
      ENDDO
      !$acc loop gang ! { dg-error "" "TODO" { xfail *-*-* } }
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
  !$acc end kernels

  !$acc kernels loop auto
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang(5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang(num:5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang(static:5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang(static:*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop seq gang ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO

  !$acc kernels loop worker
  DO i = 1,10
  ENDDO
  !$acc kernels loop worker(5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop worker(num:5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop seq worker ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang worker
  DO i = 1,10
  ENDDO

  !$acc kernels loop vector
  DO i = 1,10
  ENDDO
  !$acc kernels loop vector(5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop vector(length:5)
  DO i = 1,10
  ENDDO
  !$acc kernels loop seq vector ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang vector
  DO i = 1,10
  ENDDO
  !$acc kernels loop worker vector
  DO i = 1,10
  ENDDO

  !$acc kernels loop auto
  DO i = 1,10
  ENDDO
  !$acc kernels loop seq auto ! { dg-error "'seq' overrides other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop worker auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop vector auto ! { dg-error "'auto' conflicts with other OpenACC loop specifiers" }
  DO i = 1,10
  ENDDO
end
