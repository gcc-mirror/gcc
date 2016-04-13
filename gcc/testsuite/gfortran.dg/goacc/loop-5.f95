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

    !$acc loop tile(1)
    DO i = 1,10
    ENDDO
    !$acc loop tile(2)
    DO i = 1,10
    ENDDO
    !$acc loop tile(6-2)
    DO i = 1,10
    ENDDO
    !$acc loop tile(6+2)
    DO i = 1,10
    ENDDO
    !$acc loop tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop tile(*, 1)
    DO i = 1,10
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop tile(-1) ! { dg-warning "must be positive" }
    do i = 1,10
    enddo
    !$acc loop vector tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop worker tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop gang tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop vector gang tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop vector worker tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop gang worker tile(*)
    DO i = 1,10
    ENDDO
  !$acc end kernels


  !$acc parallel
    !$acc loop tile(1)
    DO i = 1,10
    ENDDO
    !$acc loop tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop tile(2)
    DO i = 1,10
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop tile(-1) ! { dg-warning "must be positive" }
    do i = 1,10
    enddo
    !$acc loop vector tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop worker tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop gang tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop vector gang tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop vector worker tile(*)
    DO i = 1,10
    ENDDO
    !$acc loop gang worker tile(*)
    DO i = 1,10
    ENDDO
  !$acc end parallel

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
  !$acc kernels loop gang
  DO i = 1,10
    !$acc kernels loop gang ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
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
  !$acc kernels loop worker
  DO i = 1,10
    !$acc kernels loop worker ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
    !$acc kernels loop gang ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
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
  !$acc kernels loop vector
  DO i = 1,10
    !$acc kernels loop vector ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
    !$acc kernels loop worker ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
    !$acc kernels loop gang ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
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

  !$acc kernels loop tile(1)
  DO i = 1,10
  ENDDO
  !$acc kernels loop tile(*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop tile(*, 1)
  DO i = 1,10
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc kernels loop tile(-1) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc kernels loop vector tile(*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop worker tile(*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang tile(*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop vector gang tile(*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop vector worker tile(*)
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang worker tile(*)
  DO i = 1,10
  ENDDO

  !$acc parallel loop auto
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang(static:5)
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang(static:*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang
  DO i = 1,10
    !$acc parallel loop gang ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
  ENDDO

  !$acc parallel loop worker
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker
  DO i = 1,10
    !$acc parallel loop worker ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
    !$acc parallel loop gang ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc parallel loop gang worker
  DO i = 1,10
  ENDDO

  !$acc parallel loop vector
  DO i = 1,10
    !$acc parallel loop vector ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
    !$acc parallel loop worker ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
    !$acc parallel loop gang ! { dg-error "OpenACC construct inside of non-OpenACC region" }
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc parallel loop gang vector
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker vector
  DO i = 1,10
  ENDDO

  !$acc parallel loop auto
  DO i = 1,10
  ENDDO

  !$acc parallel loop tile(1)
  DO i = 1,10
  ENDDO
  !$acc parallel loop tile(*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop tile(*, 1)
  DO i = 1,10
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc parallel loop tile(-1) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc parallel loop vector tile(*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker tile(*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang tile(*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop vector gang tile(*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop vector worker tile(*)
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang worker tile(*)
  DO i = 1,10
  ENDDO
end
