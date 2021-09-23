! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

! tests async, num_gangs, num_workers, vector_length, gang, worker, vector clauses

program test
  implicit none

  integer :: i

  !$acc parallel async
  !$acc end parallel

  !$acc parallel async(3)
  !$acc end parallel

  !$acc parallel async(i)
  !$acc end parallel

  !$acc parallel async(i+1)
  !$acc end parallel

  !$acc parallel async(-1) 
  !$acc end parallel

  !$acc parallel async(0) 
  !$acc end parallel

  !$acc parallel async() ! { dg-error "Invalid character in name" }

  !$acc parallel async(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel async(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel async("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc kernels async
  !$acc end kernels

  !$acc kernels async(3)
  !$acc end kernels

  !$acc kernels async(i)
  !$acc end kernels

  !$acc kernels async(i+1)
  !$acc end kernels

  !$acc kernels async(-1) 
  !$acc end kernels

  !$acc kernels async(0) 
  !$acc end kernels

  !$acc kernels async() ! { dg-error "Invalid character in name" }

  !$acc kernels async(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels async(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels async("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels


  !$acc parallel num_gangs ! { dg-error "Expected '\\(' after 'num_gangs'" }

  !$acc parallel num_gangs(3)
  !$acc end parallel

  !$acc parallel num_gangs(i)
  !$acc end parallel

  !$acc parallel num_gangs(i+1)
  !$acc end parallel

  !$acc parallel num_gangs(-1) ! { dg-warning "must be positive" }
  !$acc end parallel

  !$acc parallel num_gangs(0) ! { dg-warning "must be positive" }
  !$acc end parallel

  !$acc parallel num_gangs() ! { dg-error "Invalid character in name" }

  !$acc parallel num_gangs(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel num_gangs(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel num_gangs("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc kernels num_gangs ! { dg-error "Expected '\\(' after 'num_gangs'" }

  !$acc kernels num_gangs(3)
  !$acc end kernels

  !$acc kernels num_gangs(i)
  !$acc end kernels

  !$acc kernels num_gangs(i+1)
  !$acc end kernels

  !$acc kernels num_gangs(-1) ! { dg-warning "must be positive" }
  !$acc end kernels

  !$acc kernels num_gangs(0) ! { dg-warning "must be positive" }
  !$acc end kernels

  !$acc kernels num_gangs() ! { dg-error "Invalid character in name" }

  !$acc kernels num_gangs(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels num_gangs(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels num_gangs("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels


  !$acc parallel num_workers ! { dg-error "Expected '\\(' after 'num_workers'" }

  !$acc parallel num_workers(3)
  !$acc end parallel

  !$acc parallel num_workers(i)
  !$acc end parallel

  !$acc parallel num_workers(i+1)
  !$acc end parallel

  !$acc parallel num_workers(-1) ! { dg-warning "must be positive" }
  !$acc end parallel

  !$acc parallel num_workers(0) ! { dg-warning "must be positive" }
  !$acc end parallel

  !$acc parallel num_workers() ! { dg-error "Invalid expression after 'num_workers\\('" }

  !$acc parallel num_workers(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel num_workers(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel num_workers("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc kernels num_workers ! { dg-error "Expected '\\(' after 'num_workers'" }

  !$acc kernels num_workers(3)
  !$acc end kernels

  !$acc kernels num_workers(i)
  !$acc end kernels

  !$acc kernels num_workers(i+1)
  !$acc end kernels

  !$acc kernels num_workers(-1) ! { dg-warning "must be positive" }
  !$acc end kernels

  !$acc kernels num_workers(0) ! { dg-warning "must be positive" }
  !$acc end kernels

  !$acc kernels num_workers() ! { dg-error "Invalid expression after 'num_workers\\('" }

  !$acc kernels num_workers(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels num_workers(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels num_workers("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels


  !$acc parallel vector_length ! { dg-error "Expected '\\(' after 'vector_length'" }

  !$acc parallel vector_length(3)
  !$acc end parallel

  !$acc parallel vector_length(i)
  !$acc end parallel

  !$acc parallel vector_length(i+1)
  !$acc end parallel

  !$acc parallel vector_length(-1) ! { dg-warning "must be positive" }
  !$acc end parallel

  !$acc parallel vector_length(0) ! { dg-warning "must be positive" }
  !$acc end parallel

  !$acc parallel vector_length() ! { dg-error "Invalid expression after 'vector_length\\('" }

  !$acc parallel vector_length(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel vector_length(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc parallel vector_length("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end parallel

  !$acc kernels vector_length ! { dg-error "Expected '\\(' after 'vector_length'" }

  !$acc kernels vector_length(3)
  !$acc end kernels

  !$acc kernels vector_length(i)
  !$acc end kernels

  !$acc kernels vector_length(i+1)
  !$acc end kernels

  !$acc kernels vector_length(-1) ! { dg-warning "must be positive" }
  !$acc end kernels

  !$acc kernels vector_length(0) ! { dg-warning "must be positive" }
  !$acc end kernels

  !$acc kernels vector_length() ! { dg-error "Invalid expression after 'vector_length\\('" }

  !$acc kernels vector_length(1.5) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels vector_length(.true.) ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels

  !$acc kernels vector_length("1") ! { dg-error "scalar INTEGER expression" }
  !$acc end kernels


  !$acc loop gang
  do i = 1,10
  enddo
  !$acc loop gang(3)
  do i = 1,10
  enddo
  !$acc loop gang(i)
  do i = 1,10
  enddo
  !$acc loop gang(i+1)
  do i = 1,10
  enddo
  !$acc loop gang(-1) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc loop gang(0) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc loop gang() ! { dg-error "Invalid character in name" }
  do i = 1,10
  enddo
  !$acc loop gang(1.5) ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo
  !$acc loop gang(.true.) ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo
  !$acc loop gang("1") ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo


  !$acc loop worker
  do i = 1,10
  enddo
  !$acc loop worker(3)
  do i = 1,10
  enddo
  !$acc loop worker(i)
  do i = 1,10
  enddo
  !$acc loop worker(i+1)
  do i = 1,10
  enddo
  !$acc loop worker(-1) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc loop worker(0) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc loop worker() ! { dg-error "Invalid character in name" }
  do i = 1,10
  enddo
  !$acc loop worker(1.5) ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo
  !$acc loop worker(.true.) ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo
  !$acc loop worker("1") ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo


  !$acc loop vector
  do i = 1,10
  enddo
  !$acc loop vector(3)
  do i = 1,10
  enddo
  !$acc loop vector(i)
  do i = 1,10
  enddo
  !$acc loop vector(i+1)
  do i = 1,10
  enddo
  !$acc loop vector(-1) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc loop vector(0) ! { dg-warning "must be positive" }
  do i = 1,10
  enddo
  !$acc loop vector() ! { dg-error "Invalid character in name" }
  do i = 1,10
  enddo
  !$acc loop vector(1.5) ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo
  !$acc loop vector(.true.) ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo
  !$acc loop vector("1") ! { dg-error "scalar INTEGER expression" }
  do i = 1,10
  enddo

end program test
