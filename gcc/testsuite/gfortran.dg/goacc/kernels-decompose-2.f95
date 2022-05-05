! Test OpenACC 'kernels' construct decomposition.

! { dg-additional-options "-fopt-info-omp-all" }

! { dg-additional-options "--param=openacc-kernels=decompose" }
! { dg-additional-options "-O2" } for 'parloops'.

! { dg-additional-options "--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

! See also '../../c-c++-common/goacc/kernels-decompose-2.c'.

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0 c_loop_i 0 c_loop_j 0 c_loop_k 0 c_part 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

program main
  implicit none

  integer, external :: f_g
  !$acc routine (f_g) gang
  integer, external :: f_w
  !$acc routine (f_w) worker
  integer, external :: f_v
  !$acc routine (f_v) vector
  integer, external :: f_s
  !$acc routine (f_s) seq

  integer :: i, j, k
  integer :: x, y, z
  logical :: y_l
  integer, parameter :: N = 10
  integer :: a(N), b(N), c(N)

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'z' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'z' made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'y_l' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'y_l' made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'y' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'y' made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'x' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'x' made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  x = 0
  y = 0
  y_l = x < 10
  z = x
  x = x + 1
  ;
  !$acc end kernels

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-optimized {assigned OpenACC gang loop parallelism} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  do i = 1, N
     a(i) = 0
  end do
  !$acc end kernels

  !$acc kernels loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'z' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'z' already made addressable} {} { target *-*-* } l_compute$c_compute }
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     c(i) = a(i) * b(i)
  end do

  ! { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  a(z) = 0

  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     c(i) = c(i) + a(i)
  end do

  !$acc loop seq ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1 + 1, N
     c(i) = c(i) + c(i - 1)
  end do
  !$acc end kernels

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'y' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'y' already made addressable} {} { target *-*-* } l_compute$c_compute }
  !TODO What does this mean?
  !TODO { dg-optimized "assigned OpenACC worker vector loop parallelism" "" { target *-*-* } l_compute$c_compute }
  !$acc loop independent ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
     ! { dg-optimized "assigned OpenACC worker loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
     do j = 1, N
        !$acc loop independent ! { dg-line l_loop_k[incr c_loop_k] }
        ! { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k$c_loop_k }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } l_loop_k$c_loop_k }
        ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_k$c_loop_k }
        do k = 1, N
           a(1 + mod(i + j + k, N)) &
                = b(j) &
                + f_v (c(k)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
        end do
     end do
  end do

  !TODO Should the following turn into "gang-single" instead of "parloops"?
  !TODO The problem is that the first STMT is 'if (y <= 4) goto <D.2547>; else goto <D.2548>;', thus "parloops".
  ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  if (y < 5) then
     !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
     ! { dg-missed "unparallelized loop nest in OpenACC 'kernels' region: it's executed conditionally" "" { target *-*-* } l_loop_j$c_loop_j }
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
     do j = 1, N
        b(j) = f_w (c(j))
     end do
  end if
  !$acc end kernels

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'y' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'y' already made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-bogus "\[Ww\]arning: region contains gang partitioned code but is not gang partitioned" "TODO 'kernels'" { xfail *-*-* } l_compute$c_compute }
  y = f_g (a(5)) ! { dg-line l_part[incr c_part] }
  !TODO If such a construct is placed in its own part (like it is, here), can't this actually use gang paralelism, instead of "gang-single"?
  ! { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } l_part$c_part }
  ! { dg-optimized "assigned OpenACC gang worker vector loop parallelism" "" { target *-*-* } l_part$c_part }

  !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
  ! { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_j$c_loop_j }
  ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
  ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
  do j = 1, N
     b(j) = y + f_w (c(j)) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
  end do
  !$acc end kernels

  !$acc kernels ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'z' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'z' already made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'y' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'y' already made addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  y = 3

  !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
  ! { dg-note {parallelized loop nest in OpenACC 'kernels' region} {} { target *-*-* } l_loop_j$c_loop_j }
  ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
  ! { dg-optimized "assigned OpenACC gang worker loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
  do j = 1, N
     b(j) = y + f_v (c(j)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
  end do

  ! { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  z = 2
  !$acc end kernels

  ! { dg-note {beginning 'gang-single' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  !$acc kernels
  !$acc end kernels  
end program main
