! See also "../../c-c++-common/goacc/loop-2-parallel.c".

program test
  implicit none
  integer :: i, j

  !$acc parallel
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
      !$acc loop gang ! { dg-error "not allowed" }
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop seq gang ! { dg-error "conflicts with" }
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
      !$acc loop worker ! { dg-error "not allowed" }
      DO j = 1,10
      ENDDO
      !$acc loop gang ! { dg-error "not allowed" }
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop seq worker ! { dg-error "conflicts with" }
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
      !$acc loop vector ! { dg-error "not allowed" }
      DO j = 1,10
      ENDDO
      !$acc loop worker ! { dg-error "not allowed" }
      DO j = 1,10
      ENDDO
      !$acc loop gang ! { dg-error "not allowed" }
      DO j = 1,10
      ENDDO
    ENDDO
    !$acc loop seq vector ! { dg-error "conflicts with" }
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
    !$acc loop seq auto ! { dg-error "conflicts with" }
    DO i = 1,10
    ENDDO
    !$acc loop gang auto ! { dg-error "conflicts with" }
    DO i = 1,10
    ENDDO
    !$acc loop worker auto ! { dg-error "conflicts with" }
    DO i = 1,10
    ENDDO
    !$acc loop vector auto ! { dg-error "conflicts with" }
    DO i = 1,10
    ENDDO
  !$acc end parallel

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
  !$acc parallel loop seq gang ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop worker
  DO i = 1,10
  ENDDO
  !$acc parallel loop seq worker ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang worker
  DO i = 1,10
  ENDDO

  !$acc parallel loop vector
  DO i = 1,10
  ENDDO
  !$acc parallel loop seq vector ! { dg-error "conflicts with" }
  DO i = 1,10
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
  !$acc parallel loop seq auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop vector auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
end
