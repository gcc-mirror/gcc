! Exercise nested function decomposition, gcc/tree-nested.c.
! See gcc/testsuite/gcc.dg/goacc/nested-function-1.c for the C version.

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

    !$acc kernels loop &
    !$acc gang(num:local_arg) worker(local_arg) vector(local_arg) &
    !$acc wait async(local_arg)
    do local_i = 1, N
       !$acc cache (local_a(local_i:local_i + 5))
       local_a(local_i) = 100
       !$acc loop seq tile(*)
       do local_j = 1, N
       enddo
       !$acc loop auto independent tile(1)
       do local_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc kernels loop &
    !$acc gang(static:local_arg) worker(local_arg) vector(local_arg) &
    !$acc wait(local_arg, local_arg + 1, local_arg + 2) async
    do local_i = 1, N
       !$acc cache (local_a(local_i:local_i + 4))
       local_a(local_i) = 100
       !$acc loop seq tile(1)
       do local_j = 1, N
       enddo
       !$acc loop auto independent tile(*)
       do local_j = 1, N
       enddo
    enddo
    !$acc end kernels loop
  end subroutine local

  subroutine nonlocal ()
    nonlocal_a (:) = 5
    nonlocal_arg = 5

    !$acc kernels loop &
    !$acc gang(num:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) &
    !$acc wait async(nonlocal_arg)
    do nonlocal_i = 1, N
       !$acc cache (nonlocal_a(nonlocal_i:nonlocal_i + 3))
       nonlocal_a(nonlocal_i) = 100
       !$acc loop seq tile(2)
       do nonlocal_j = 1, N
       enddo
       !$acc loop auto independent tile(3)
       do nonlocal_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc kernels loop &
    !$acc gang(static:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) &
    !$acc wait(nonlocal_arg, nonlocal_arg + 1, nonlocal_arg + 2) async
    do nonlocal_i = 1, N
       !$acc cache (nonlocal_a(nonlocal_i:nonlocal_i + 2))
       nonlocal_a(nonlocal_i) = 100
       !$acc loop seq tile(*)
       do nonlocal_j = 1, N
       enddo
       !$acc loop auto independent tile(*)
       do nonlocal_j = 1, N
       enddo
    enddo
    !$acc end kernels loop
  end subroutine nonlocal
end program main
