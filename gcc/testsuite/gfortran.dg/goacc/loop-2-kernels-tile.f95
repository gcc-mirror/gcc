program test
  implicit none
  integer :: i, j

  !$acc kernels
    !$acc loop tile ! { dg-error "Failed to match clause" }
    DO i = 1,10
    ENDDO
    !$acc loop tile() ! { dg-error "Syntax error" }
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
    !$acc loop tile(i) ! { dg-error "constant expression" }
    do i = 1,10
    enddo
    !$acc loop tile(2, 2, 1) ! { dg-error "not enough DO loops for tiled" }
      do i = 1, 3
          do j = 4, 6
          end do
      end do    
      !$acc loop tile(2, 2)
      do i = 1, 5, 2
          do j = i + 1, 7, i  ! { dg-error "tiled loops don.t form rectangular iteration space" }
          end do
      end do
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

  !$acc kernels loop tile ! { dg-error "Failed to match clause" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop tile() ! { dg-error "Syntax error" }
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
  !$acc kernels loop tile(i) ! { dg-error "constant expression" }
  do i = 1,10
  enddo
  !$acc kernels loop tile(2, 2, 1) ! { dg-error "not enough DO loops for tiled" }
    do i = 1, 3
        do j = 4, 6
        end do
    end do    
    !$acc kernels loop tile(2, 2)
    do i = 1, 5, 2
        do j = i + 1, 7, i  ! { dg-error "tiled loops don.t form rectangular iteration space" }
        end do
    end do
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
end
