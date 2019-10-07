subroutine parloop
  integer, parameter :: n = 100
  integer i, j, k, a

  !$acc parallel loop tile(10)
  do i = 1, n
  end do
  
  !$acc parallel loop tile(*)
  do i = 1, n
  end do

  !$acc parallel loop tile(10, *)
  do i = 1, n
     do j = 1, n
     end do
  end do
 
  !$acc parallel loop tile(10, *, i) ! { dg-error "" }
  do i = 1, n
     do j = 1, n
        do k = 1, n
        end do
     end do
  end do 

  !$acc parallel loop tile ! { dg-error "Failed to match clause" }
  do i = 1, n
  end do

  !$acc parallel loop tile() ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc parallel loop tile(,1) ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc parallel loop tile(,,) ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc parallel loop tile(1.1) ! { dg-error "requires a scalar INTEGER" }
  do i = 1, n
  end do

  !$acc parallel loop tile(-3) ! { dg-warning "must be positive" }
  do i = 1, n
  end do

  !$acc parallel loop tile(10, -3) ! { dg-warning "must be positive" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc parallel loop tile(-100, 10, 5) ! { dg-warning "must be positive" }
  do i = 1, n
     do j = 1, n
        do k = 1, n
        end do
     end do
  end do 

  !$acc parallel loop tile(10, .true.) ! { dg-error "requires a scalar" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc parallel loop tile(1, a) ! { dg-error "constant expression" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc parallel loop tile(a, 1) ! { dg-error "constant expression" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc parallel loop tile(2, 3) collapse (2) ! { dg-error "Incompatible use" }
  do i = 1, n
     do j = 1, n
     end do
  end do
end subroutine parloop

subroutine par
  integer, parameter :: n = 100
  integer i, j, k

  !$acc parallel
  !$acc loop tile ! { dg-error "Failed to match clause" }
  do i = 1, n
  end do

  !$acc loop tile() ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc loop tile(1)
  do i = 1, n
  end do

  !$acc loop tile(*)
  do i = 1, n
  end do

  !$acc loop tile(2)
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc loop tile(-2) ! { dg-warning "must be positive" }
  do i = 1, n
  end do

  !$acc loop tile(i) ! { dg-error "constant expression" }
  do i = 1, n
  end do

  !$acc loop tile(2, 2, 1)
  do i = 1, n
     do j = 1, n
        do k = 1, n
        end do
     end do
  end do 

  !$acc parallel loop tile(2, 2)
  do i = 1, n
     do j = i+1, n, j ! { dg-error "rectangular iteration space" }
     end do
  end do

  !$acc loop vector tile(*)
  do i = 1, n
  end do
  
  !$acc loop worker tile(*)
  do i = 1, n
  end do

  !$acc loop gang tile(*)
  do i = 1, n
  end do

  !$acc loop vector gang tile(*)
  do i = 1, n
  end do

  !$acc loop vector worker tile(*)
  do i = 1, n
  end do

  !$acc loop gang worker tile(*)
  do i = 1, n
  end do

  !$acc loop tile(2, 3) collapse (2) ! { dg-error "Incompatible use" }
  do i = 1, n
     do j = 1, n
     end do
  end do
  !$acc end parallel
end subroutine par

subroutine kern
  integer, parameter :: n = 100
  integer i, j, k

  !$acc kernels
  !$acc loop tile  ! { dg-error "Failed to match clause" }
  do i = 1, n
  end do

  !$acc loop tile() ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc loop tile(1)
  do i = 1, n
  end do

  !$acc loop tile(*)
  do i = 1, n
  end do

  !$acc loop tile(2)
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc loop tile(-2) ! { dg-warning "must be positive" }
  do i = 1, n
  end do

  !$acc loop tile(i) ! { dg-error "constant expression" }
  do i = 1, n
  end do

  !$acc loop tile(2, 2, 1)
  do i = 1, n
     do j = 1, n
        do k = 1, n
        end do
     end do
  end do 

  !$acc parallel loop tile(2, 2)
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc loop vector tile(*)
  do i = 1, n
  end do
  
  !$acc loop worker tile(*)
  do i = 1, n
  end do

  !$acc loop gang tile(*)
  do i = 1, n
  end do

  !$acc loop vector gang tile(*)
  do i = 1, n
  end do

  !$acc loop vector worker tile(*)
  do i = 1, n
  end do

  !$acc loop gang worker tile(*)
  do i = 1, n
  end do

  !$acc loop tile(2, 3) collapse (2) ! { dg-error "Incompatible use" }
  do i = 1, n
     do j = 1, n
     end do
  end do
  !$acc end kernels
end subroutine kern

subroutine kernsloop
  integer, parameter :: n = 100
  integer i, j, k, a

  !$acc kernels loop tile(10)
  do i = 1, n
  end do
  
  !$acc kernels loop tile(*)
  do i = 1, n
  end do

  !$acc kernels loop tile(10, *)
  do i = 1, n
     do j = 1, n
     end do
  end do
 
  !$acc kernels loop tile(10, *, i) ! { dg-error "" }
  do i = 1, n
     do j = 1, n
        do k = 1, n
        end do
     end do
  end do 

  !$acc kernels loop tile ! { dg-error "Failed to match clause" }
  do i = 1, n
  end do

  !$acc kernels loop tile() ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc kernels loop tile(,1) ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc kernels loop tile(,,) ! { dg-error "Syntax error" }
  do i = 1, n
  end do

  !$acc kernels loop tile(1.1) ! { dg-error "requires a scalar INTEGER" }
  do i = 1, n
  end do

  !$acc kernels loop tile(-3) ! { dg-warning "must be positive" }
  do i = 1, n
  end do

  !$acc kernels loop tile(10, -3) ! { dg-warning "must be positive" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc kernels loop tile(-100, 10, 5) ! { dg-warning "must be positive" }
  do i = 1, n
     do j = 1, n
        do k = 1, n
        end do
     end do
  end do 

  !$acc kernels loop tile(10, .true.) ! { dg-error "requires a scalar" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc kernels loop tile(1, a) ! { dg-error "constant expression" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc kernels loop tile(a, 1) ! { dg-error "constant expression" }
  do i = 1, n
     do j = 1, n
     end do
  end do

  !$acc kernels loop tile(2, 3) collapse (2) ! { dg-error "Incompatible use" }
  do i = 1, n
     do j = 1, n
     end do
  end do
end subroutine kernsloop
