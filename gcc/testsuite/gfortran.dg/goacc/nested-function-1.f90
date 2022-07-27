! Exercise nested function decomposition, gcc/tree-nested.c.
! See gcc/testsuite/gcc.dg/goacc/nested-function-1.c for the C version.

! { dg-additional-options "--param=openacc-kernels=decompose" }

! { dg-additional-options "-fopt-info-all-omp" }

! { dg-additional-options "--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute_loop 0 c_loop 0] }
! { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

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

    !$acc update device(local_a) if_present

    !$acc kernels loop &
    !$acc gang(num:local_arg) worker(local_arg) vector(local_arg) &
    !$acc wait async(local_arg) ! { dg-line l_compute_loop[incr c_compute_loop] }
    ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'local_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'local_i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'local_i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop }
    do local_i = 1, N
       !$acc cache (local_a(local_i:local_i + 5))
       local_a(local_i) = 100
       !$acc loop seq tile(*) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do local_j = 1, N
       enddo
       !$acc loop auto independent tile(1) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do local_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc kernels loop &
    !$acc gang(static:local_arg) worker(local_arg) vector(local_arg) &
    !$acc wait(local_arg, local_arg + 1, local_arg + 2) async ! { dg-line l_compute_loop[incr c_compute_loop] }
    ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'local_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'local_i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'local_i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop }
    do local_i = 1, N
       !$acc cache (local_a(local_i:local_i + 4))
       local_a(local_i) = 100
       !$acc loop seq tile(1) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do local_j = 1, N
       enddo
       !$acc loop auto independent tile(*) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'local_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do local_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc exit data copyout(local_a) delete(local_i) finalize
  end subroutine local

  subroutine nonlocal ()
    nonlocal_a (:) = 5
    nonlocal_arg = 5

    !$acc update device(nonlocal_a) if_present

    !$acc kernels loop &
    !$acc gang(num:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) &
    !$acc wait async(nonlocal_arg) ! { dg-line l_compute_loop[incr c_compute_loop] }
    ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'nonlocal_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'nonlocal_i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'nonlocal_i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop }
    do nonlocal_i = 1, N
       !$acc cache (nonlocal_a(nonlocal_i:nonlocal_i + 3))
       nonlocal_a(nonlocal_i) = 100
       !$acc loop seq tile(2) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do nonlocal_j = 1, N
       enddo
       !$acc loop auto independent tile(3) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do nonlocal_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc kernels loop &
    !$acc gang(static:nonlocal_arg) worker(nonlocal_arg) vector(nonlocal_arg) &
    !$acc wait(nonlocal_arg, nonlocal_arg + 1, nonlocal_arg + 2) async ! { dg-line l_compute_loop[incr c_compute_loop] }
    ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'nonlocal_i\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'nonlocal_i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'nonlocal_i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute_loop$c_compute_loop }
    ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_compute_loop$c_compute_loop }
    do nonlocal_i = 1, N
       !$acc cache (nonlocal_a(nonlocal_i:nonlocal_i + 2))
       nonlocal_a(nonlocal_i) = 100
       !$acc loop seq tile(*) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do nonlocal_j = 1, N
       enddo
       !$acc loop auto independent tile(*) ! { dg-line l_loop[incr c_loop] }
       ! { dg-note {variable 'nonlocal_j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop$c_loop }
       do nonlocal_j = 1, N
       enddo
    enddo
    !$acc end kernels loop

    !$acc exit data copyout(nonlocal_a) delete(nonlocal_i) finalize
  end subroutine nonlocal
end program main
