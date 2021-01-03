! See also "../../c-c++-common/goacc/loop-3.c".

program test
  implicit none
  integer :: i

  !$acc parallel
    !$acc loop gang(5) ! { dg-error "argument not permitted" }
    DO i = 1,10
    ENDDO

    !$acc loop gang(num:5) ! { dg-error "argument not permitted" }
    DO i = 1,10
    ENDDO

    !$acc loop worker(5) ! { dg-error "argument not permitted" }
    DO i = 1,10
    ENDDO

    !$acc loop worker(num:5) ! { dg-error "argument not permitted" }
    DO i = 1,10
    ENDDO

    !$acc loop vector(5) ! { dg-error "argument not permitted" }
    DO i = 1,10
    ENDDO

    !$acc loop vector(length:5) ! { dg-error "argument not permitted" }
    DO i = 1,10
    ENDDO
  !$acc end parallel

  !$acc parallel loop gang(5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop gang(num:5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop worker(5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop worker(num:5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop vector(5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop vector(length:5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO
end
