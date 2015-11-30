! Exercise how tree-nested.c handles gang, worker vector and seq.

! { dg-do compile } 

program main
  integer, parameter :: N = 100
  integer :: nonlocal_arg
  integer :: nonlocal_a(N)
  integer :: nonlocal_i
  integer :: nonlocal_j
  
  nonlocal_a (:) = 5
  nonlocal_arg = 5
  
  call local ()
  call nonlocal ()

contains

  subroutine local ()
    integer :: local_i
    integer :: local_arg
    integer :: local_a(N)
    integer :: local_j
    
    local_a (:) = 5
    local_arg = 5

    !$acc kernels loop gang(num:local_arg) worker(local_arg) vector(local_arg)
    do local_i = 1, N
       local_a(local_i) = 100
       !$acc loop seq
       do local_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc kernels loop gang(static:local_arg) worker(local_arg) &
    !$acc vector(local_arg)
    do local_i = 1, N
       local_a(local_i) = 100
       !$acc loop seq
       do local_j = 1, N
       enddo
    enddo
    !$acc end kernels loop
  end subroutine local

  subroutine nonlocal ()
    nonlocal_a (:) = 5
    nonlocal_arg = 5
  
    !$acc kernels loop gang(num:nonlocal_arg) worker(nonlocal_arg) &
    !$acc vector(nonlocal_arg)
    do nonlocal_i = 1, N
       nonlocal_a(nonlocal_i) = 100
       !$acc loop seq
       do nonlocal_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc kernels loop gang(static:nonlocal_arg) worker(nonlocal_arg) &
    !$acc vector(nonlocal_arg)
    do nonlocal_i = 1, N
       nonlocal_a(nonlocal_i) = 100
       !$acc loop seq
       do nonlocal_j = 1, N
       enddo
    enddo
    !$acc end kernels loop
  end subroutine nonlocal
end program main
