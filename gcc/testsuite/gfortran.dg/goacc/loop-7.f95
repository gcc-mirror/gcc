! { dg-do compile }
! { dg-additional-options "-fmax-errors=100" }

program test
  implicit none
  integer :: i, j, static, num, length

  !$acc kernels
    !$acc loop gang(static:static)
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:*)
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:1)
    DO i = 1,10
    ENDDO
    !$acc loop gang(,static:1) ! { dg-error "Invalid character" }
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:1,) ! { dg-error "Invalid character" }
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:*, num:5)
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:1, 5)
    DO i = 1,10
    ENDDO
    !$acc loop gang(num:num, static:1)
    DO i = 1,10
    ENDDO
    !$acc loop gang(static:*, num:5, static:5) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop gang(1, num:2, static:3) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop gang(num:num static:1) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop gang(num)
    DO i = 1,10
    ENDDO
    !$acc loop gang(num:num+1, static:1+num)
    DO i = 1,10
    ENDDO
    !$acc loop gang(length:num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO

    !$acc loop worker
    DO i = 1,10
    ENDDO
    !$acc loop worker (5)
    DO i = 1,10
    ENDDO
    !$acc loop worker (num)
    DO i = 1,10
    ENDDO
    !$acc loop worker (static:num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop worker (num:,) ! { dg-error "Invalid character" }
    DO i = 1,10
    ENDDO
    !$acc loop worker (num:num:num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop worker (num:num*num)
    DO i = 1,10
    ENDDO
    !$acc loop worker (length:num*num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop worker (num:*) ! { dg-error "Invalid character" }
    DO i = 1,10
    ENDDO
    !$acc loop worker (num:5)
    DO i = 1,10
    ENDDO

    !$acc loop vector
    DO i = 1,10
    ENDDO
    !$acc loop vector (32)
    DO i = 1,10
    ENDDO
    !$acc loop vector (length)
    DO i = 1,10
    ENDDO
    !$acc loop vrctor (static:num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop vector (length:,) ! { dg-error "Invalid character" }
    DO i = 1,10
    ENDDO
    !$acc loop vector (length:num:num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop vector (length:static*num)
    DO i = 1,10
    ENDDO
    !$acc loop vector (length:length)
    DO i = 1,10
    ENDDO
    !$acc loop vector (length:32)
    DO i = 1,10
    ENDDO
    !$acc loop vector (num:num*num) ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop vector (length:*) ! { dg-error "Invalid character" }
    DO i = 1,10
    ENDDO


    !$acc loop auto
    DO i = 1,10
    ENDDO
  !$acc end kernels
end
