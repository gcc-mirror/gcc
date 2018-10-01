! Verify the invalid gang, worker, vector parallelism error messages.

program np
  integer, parameter :: n = 100
  integer :: i, j, k

  !$acc parallel loop gang
  do i = 1, n
     !$acc loop gang ! { dg-error "gangs is not allowed inside another loop parallelized across gangs" }
     do j = 1, n
     end do

     !$acc loop worker
     do j = 1, n
     end do

     !$acc loop vector
     do j = 1, n
     end do     
  end do
  
  !$acc parallel loop worker
  do i = 1, n
     !$acc loop gang ! { dg-error "gangs is not allowed inside loop parallelized across workers" }
     do j = 1, n
     end do

     !$acc loop worker ! { dg-error "workers is not allowed inside another loop parallelized across workers" }
     do j = 1, n
     end do

     !$acc loop vector
     do j = 1, n
     end do     
  end do

  !$acc parallel loop vector
  do i = 1, n
     !$acc loop gang ! { dg-error "gangs is not allowed inside loop parallelized across vectors" }
     do j = 1, n
     end do

     !$acc loop worker ! { dg-error "workers is not allowed inside another loop parallelized across vectors" }
     do j = 1, n
     end do

     !$acc loop vector ! { dg-error "vectors is not allowed inside another loop parallelized across vectors" }
     do j = 1, n
     end do     
  end do
end program np
