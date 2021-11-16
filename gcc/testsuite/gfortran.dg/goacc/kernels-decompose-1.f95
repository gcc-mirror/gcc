! Test OpenACC 'kernels' construct decomposition.

! { dg-additional-options "-fopt-info-optimized-note-omp" }
! { dg-additional-options "-O2" } for "Graphite".
! { dg-additional-options "-fdump-tree-omp_oacc_kernels_decompose-details -fdump-tree-gimple-details" }
! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

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
  integer :: sum

  !$acc kernels
  ! { dg-optimized {'map\(force_tofrom:x \[len: [0-9]+\]\[implicit\]\)' optimized to 'map\(to:x \[len: [0-9]+\]\[implicit\]\)'} "" { target *-*-* } .-1 }
  ! { dg-optimized {'map\(to:x \[len: [0-9]+\]\[implicit\]\)' further optimized to 'private\(x\)'} "" { target *-*-* } .-2 }
  ! { dg-optimized {'map\(force_tofrom:y_l \[len: [0-9]+\]\[implicit\]\)' optimized to 'map\(to:y_l \[len: [0-9]+\]\[implicit\]\)'} "" { target *-*-* } .-3 }
  ! { dg-optimized {'map\(to:y_l \[len: [0-9]+\]\[implicit\]\)' further optimized to 'private\(y_l\)'} "" { target *-*-* } .-4 }
  x = 0
  y = 0
  y_l = x < 10
  z = x
  x = x + 1
  ;
  !$acc end kernels

  !$acc kernels
  do i = 1, N ! { dg-optimized "assigned OpenACC gang vector loop parallelism" }
     a(i) = 0
  end do
  !$acc end kernels

  !$acc kernels loop
  ! { dg-bogus "assigned OpenACC seq loop parallelism" "TODO-kernels Graphite cannot represent access function" { xfail *-*-* } .-1 }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "TODO-kernels Graphite cannot represent access function " { xfail *-*-* } .-2 }
  ! { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-3 }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc kernels ! { dg-optimized {.map\(force_tofrom:sum \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:sum \[len: [0-9]+\]\[implicit\]\).} }
  !$acc loop
  ! { dg-bogus "assigned OpenACC seq loop parallelism" "TODO-kernels Graphite cannot represent access function" { xfail *-*-* } .-1 }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "TODO-kernels Graphite cannot represent access function " { xfail *-*-* } .-2 }
  ! { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-3 }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc loop
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "TODO-kernels Graphite cannot represent access function " { target *-*-* } .-1 }
  ! { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 }
  do i = 1, N
     c(i) = a(i) * b(i)
  end do

  a(z) = 0

  !$acc loop
  ! { dg-bogus "assigned OpenACC seq loop parallelism" "TODO-kernels missing synth reductions" { xfail *-*-* } .-1 }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "TODO-kernels missing synth reductions" { xfail *-*-* } .-2 }
  ! { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-3 }
  do i = 1, N
    sum = sum + a(i)
  end do

  !$acc loop seq ! { dg-optimized "assigned OpenACC seq loop parallelism" }
  do i = 1 + 1, N
     c(i) = c(i) + c(i - 1)
  end do
  !$acc end kernels

  !$acc kernels
  !$acc loop independent ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, N
     !$acc loop independent ! { dg-optimized "assigned OpenACC worker loop parallelism" }
     do j = 1, N
        !$acc loop independent ! { dg-optimized "assigned OpenACC seq loop parallelism" }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
        ! { dg-bogus "optimized: assigned OpenACC vector loop parallelism" "" { target *-*-* } .-2 }
        do k = 1, N
           a(1 + mod(i + j + k, N)) &
                = b(j) &
                + f_v (c(k)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
        end do
     end do
  end do

  if (y < 5) then
     !$acc loop independent ! { dg-optimized "assigned OpenACC gang loop parallelism" }
     do j = 1, N
        b(j) = f_w (c(j)) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
     end do
  end if
  !$acc end kernels

  !$acc kernels
  y = f_g (a(5)) ! { dg-optimized "assigned OpenACC gang worker vector loop parallelism" }

  !$acc loop independent ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  ! { dg-bogus "optimized: assigned OpenACC gang vector loop parallelism" "" { target *-*-* } .-1 }
  do j = 1, N
     b(j) = y + f_w (c(j)) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
  end do
  !$acc end kernels

  !$acc kernels
  ! { dg-optimized {.map\(force_tofrom:z \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:z \[len: [0-9]+\]\[implicit\]\).} "" { target *-*-* } .-1 }
  ! { dg-optimized {.map\(to:z \[len: [0-9]+\]\[implicit\]\). further optimized to .private\(z\).} "" { target *-*-* } .-2 }
  ! { dg-optimized {.map\(force_tofrom:y \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:y \[len: [0-9]+\]\[implicit\]\).} "" { target *-*-* } .-3 }
  ! { dg-optimized {.map\(to:y \[len: [0-9]+\]\[implicit\]\). further optimized to .private\(y\).} "" { target *-*-* } .-4 }
  y = 3

  !$acc loop independent ! { dg-optimized "assigned OpenACC gang worker loop parallelism" }
  ! { dg-bogus "optimized: assigned OpenACC gang vector loop parallelism" "" { target *-*-* } .-1 }
  do j = 1, N
     b(j) = y + f_v (c(j)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
  end do

  z = 2
  !$acc end kernels

  !$acc kernels
  !$acc end kernels  
end program main

! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(i\)$} 4 "gimple" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(i\) independent$} 1 "gimple" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop auto private\(i\)$} 1 "gimple" } }

! Check that the OpenACC 'kernels' got decomposed into 'data' and an enclosed
! sequence of compute constructs.
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_data_kernels} 8 "omp_oacc_kernels_decompose" } }
!
! 'data' plus five CCs.
! { dg-final { scan-tree-dump-times {(?n)#pragma omp target oacc_parallel_kernels_graphite async\(-1\)} 6 "omp_oacc_kernels_decompose" } }

! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop auto private\(i\)$} 5 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(i\) seq$} 1 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(i\) independent$} 1 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(j\) independent$} 4 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop private\(k\) independent$} 1 "omp_oacc_kernels_decompose" } }
! { dg-final { scan-tree-dump-times {(?n)#pragma acc loop} 12 "omp_oacc_kernels_decompose" } }

! Each of the parallel regions is async, and there is a final call to
! __builtin_GOACC_wait.
! { dg-final { scan-tree-dump-times "__builtin_GOACC_wait" 4 "omp_oacc_kernels_decompose" } }
