! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

! TODO: nested kernels are allowed in 2.0

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
    !$acc loop vector(5)
    DO i = 1,10
    ENDDO
    !$acc loop vector(length:5)
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

    !$acc loop tile ! { dg-error "Unclassifiable" }
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


  !$acc parallel
    !$acc loop auto
    DO i = 1,10
    ENDDO
    !$acc loop gang
    DO i = 1,10
    ENDDO
    !$acc loop gang(5) ! { dg-error "num arguments" }
    DO i = 1,10
    ENDDO
    !$acc loop gang(num:5) ! { dg-error "num arguments" }
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
    !$acc loop worker(5) ! { dg-error "num arguments" }
    DO i = 1,10
    ENDDO
    !$acc loop worker(num:5) ! { dg-error "num arguments" }
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
    !$acc loop vector(5) ! { dg-error "length arguments" }
    DO i = 1,10
    ENDDO
    !$acc loop vector(length:5) ! { dg-error "length arguments" }
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

    !$acc loop tile ! { dg-error "Unclassifiable" }
    DO i = 1,10
    ENDDO
    !$acc loop tile() ! { dg-error "Syntax error" }
    DO i = 1,10
    ENDDO
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
    !$acc kernels loop gang 
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc kernels loop seq gang ! { dg-error "conflicts with" }
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
  !$acc kernels loop worker
  DO i = 1,10
    !$acc kernels loop worker 
    DO j = 1,10
    ENDDO
    !$acc kernels loop gang 
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc kernels loop seq worker ! { dg-error "conflicts with" }
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
  !$acc kernels loop vector
  DO i = 1,10
    !$acc kernels loop vector 
    DO j = 1,10
    ENDDO
    !$acc kernels loop worker 
    DO j = 1,10
    ENDDO
    !$acc kernels loop gang 
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc kernels loop seq vector ! { dg-error "conflicts with" }
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
  !$acc kernels loop seq auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop gang auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop worker auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO
  !$acc kernels loop vector auto ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO

  !$acc kernels loop tile ! { dg-error "Unclassifiable" }
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

  !$acc parallel loop auto
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang(5) ! { dg-error "num arguments" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop gang(num:5) ! { dg-error "num arguments" }
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
    !$acc parallel loop gang 
    DO j = 1,10
    ENDDO
  ENDDO
  !$acc parallel loop seq gang ! { dg-error "conflicts with" }
  DO i = 1,10
  ENDDO

  !$acc parallel loop worker
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker(5) ! { dg-error "num arguments" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker(num:5) ! { dg-error "num arguments" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop worker
  DO i = 1,10
    !$acc parallel loop worker 
    DO j = 1,10
    ENDDO
    !$acc parallel loop gang 
    DO j = 1,10
    ENDDO
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
  !$acc parallel loop vector(5) ! { dg-error "length arguments" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop vector(length:5) ! { dg-error "length arguments" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop vector
  DO i = 1,10
    !$acc parallel loop vector 
    DO j = 1,10
    ENDDO
    !$acc parallel loop worker 
    DO j = 1,10
    ENDDO
    !$acc parallel loop gang 
    DO j = 1,10
    ENDDO
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

  !$acc parallel loop tile ! { dg-error "Unclassifiable" }
  DO i = 1,10
  ENDDO
  !$acc parallel loop tile() ! { dg-error "Syntax error" }
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
  !$acc parallel loop tile(i) ! { dg-error "constant expression" }
  do i = 1,10
  enddo
  !$acc parallel loop tile(2, 2, 1) ! { dg-error "not enough DO loops for tiled" }
    do i = 1, 3
        do j = 4, 6
        end do
    end do    
    !$acc parallel loop tile(2, 2)
    do i = 1, 5, 2
        do j = i + 1, 7, i  ! { dg-error "tiled loops don.t form rectangular iteration space" }
        end do
    end do
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
