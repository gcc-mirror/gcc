! See also "../../c-c++-common/goacc/loop-3-serial.c".

program test
  implicit none
  integer :: i

  !$acc serial
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
  !$acc end serial

  !$acc serial loop gang(5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc serial loop gang(num:5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc serial loop worker(5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc serial loop worker(num:5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc serial loop vector(5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO

  !$acc serial loop vector(length:5) ! { dg-error "argument not permitted" }
  DO i = 1,10
  ENDDO
end
