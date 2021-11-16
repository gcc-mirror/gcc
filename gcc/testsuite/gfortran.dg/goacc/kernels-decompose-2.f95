! Test OpenACC 'kernels' construct decomposition.

! { dg-additional-options "-fopenacc-kernels-annotate-loops" }
! { dg-additional-options "-fopt-info-omp-all" }

! { dg-additional-options "--param=openacc-kernels=decompose" }
! { dg-additional-options "-O2" } for 'Graphite'.

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

  !$acc kernels ! { dg-line l_kernels[incr region] }
  ! { dg-missed {.map\(force_tofrom:y \[len: [0-9]+\]\[implicit\]\). not optimized: .y. used} "" { target *-*-* } l_kernels$region }
  ! { dg-optimized {.map\(force_tofrom:y_l \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:y_l \[len: [0-9]+\]\[implicit\]\).} "" { target *-*-* } l_kernels$region }
  ! { dg-optimized {.map\(to:y_l \[len: [0-9]+\]\[implicit\]\). further optimized to .private\(y_l\).}  "" { target *-*-* } l_kernels$region }
  ! { dg-optimized {.map\(force_tofrom:x \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:x \[len: [0-9]+\]\[implicit\]\).} "" { target *-*-* } l_kernels$region }
  ! { dg-optimized {.map\(to:x \[len: [0-9]+\]\[implicit\]\). further optimized to .private\(x\).}  "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(force_tofrom:z \[len: [0-9]+\]\[implicit\]\). not optimized: .z. used} "" { target *-*-* } l_kernels$region }
  x = 0
  y = 0
  y_l = x < 10
  z = x
  x = x + 1

  ;
  !$acc end kernels

  !$acc kernels ! { dg-line l_kernels[incr region] }
  ! { dg-missed {'map\(tofrom:a \[len: [0-9]+\]\[implicit\]\)' not optimized: 'a' is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  do i = 1, N  ! { dg-line l_loop_i[incr c_loop_i] }
     ! { dg-message "note: forwarded loop nest in OpenACC 'kernels' region to 'Graphite' for analysis" "" { target *-*-* } l_loop_i$c_loop_i }
     ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
     a(i) = 0
  end do
  !$acc end kernels

  !$acc kernels loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-missed {.map\(tofrom:b \[len: [0-9]+\]\[implicit\]\). not optimized: .b. is unsuitable for privatization} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-missed {.map\(tofrom:a \[len: [0-9]+\]\[implicit\]\). not optimized: .a. is unsuitable for privatization} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-message "note: forwarded loop nest in OpenACC 'kernels' region to 'Graphite' for analysis" "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang loop parallelism" "TODO Graphite cannot represent access function" { xfail *-*-* } l_loop_i$c_loop_i }
  ! { dg-bogus "assigned OpenACC seq loop parallelism" "TODO Graphite cannot represent access function" { xfail *-*-* } l_loop_i$c_loop_i }
  ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc kernels ! { dg-line l_kernels[incr region] }
  ! { dg-missed {.map\(force_tofrom:z \[len: [0-9]+\]\[implicit\]\). not optimized: .z. used} "" { target *-*-* } l_kernels$region }
  ! { dg-missed    {\.\.\. here}  "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(tofrom:c \[len: [0-9]+\]\[implicit\]\). not optimized: .c. is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(tofrom:b \[len: [0-9]+\]\[implicit\]\). not optimized: .b. is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(tofrom:a \[len: [0-9]+\]\[implicit\]\). not optimized: .a. is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-message "note: forwarded loop nest in OpenACC 'kernels' region to 'Graphite' for analysis" "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { xfail *-*-* } l_loop_i$c_loop_i }
  ! { dg-bogus "assigned OpenACC seq loop parallelism" "TODO Graphite cannot represent access function" { xfail *-*-* } l_loop_i$c_loop_i }
  ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'Graphite' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     c(i) = a(i) * b(i)
  end do

  a(z) = 0

  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'Graphite' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     c(i) = c(i) + a(i)
  end do

  !$acc loop seq ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1 + 1, N
     c(i) = c(i) + c(i - 1)
  end do
  !$acc end kernels

  !$acc kernels ! { dg-line l_kernels[incr region] }
  ! { dg-missed {.map\(force_tofrom:y \[len: [0-9]+\]\[implicit\]\). not optimized: .y. used} "" { target *-*-* } l_kernels$region }
  ! { dg-missed    {\.\.\. here}  "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(tofrom:c \[len: [0-9]+\]\[implicit\]\). not optimized: .c. is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(tofrom:b \[len: [0-9]+\]\[implicit\]\). not optimized: .b. is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {.map\(tofrom:a \[len: [0-9]+\]\[implicit\]\). not optimized: .a. is unsuitable for privatization} "" { target *-*-* } l_kernels$region }

  !$acc loop independent ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-bogus "missed: .independent. loop in .kernels. region has not been analyzed .cf. .graphite. dumps for more information.." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_i$c_loop_i }
  ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
     ! { dg-optimized "assigned OpenACC worker loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
     ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
     ! { dg-bogus "missed: .independent. loop in .kernels. region has not been analyzed .cf. .graphite. dumps for more information.." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
     do j = 1, N
        !$acc loop independent ! { dg-line l_loop_k[incr c_loop_k] }
        ! { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_k$c_loop_k }
        ! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } l_loop_k$c_loop_k }
        ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_k$c_loop_k }
        ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_k$c_loop_k }
        ! { dg-bogus "missed: .independent. loop in .kernels. region has not been analyzed .cf. .graphite. dumps for more information.." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_k$c_loop_k }
        do k = 1, N
           a(1 + mod(i + j + k, N)) &
                = b(j) &
                + f_v (c(k)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
        end do
     end do
  end do

  if (y < 5) then ! { dg-message "note: beginning 'Graphite' part in OpenACC 'kernels' region" }
     !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
     ! { dg-missed "unparallelized loop nest in OpenACC 'kernels' region: it's executed conditionally" "" { target *-*-* } l_loop_j$c_loop_j } ! TODO-kernels Clarify: should this be unparallelized or should the warning go away?
     ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
     ! { dg-bogus "missed: .independent. loop in .kernels. region has not been analyzed .cf. .graphite. dumps for more information.." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
     ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
     do j = 1, N
        b(j) = f_w (c(j)) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
     end do
  end if
  !$acc end kernels

  !$acc kernels ! { dg-line l_kernels[incr region] }
  ! { dg-missed {'map\(force_tofrom:y \[len: [0-9]+\]\[implicit\]\)' not optimized: 'y' used} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {'map\(tofrom:c \[len: [0-9]+\]\[implicit\]\)' not optimized: 'c' is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {'map\(tofrom:b \[len: [0-9]+\]\[implicit\]\)' not optimized: 'b' is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  ! { dg-missed {'map\(tofrom:a \[len: [0-9]+\]\[implicit\]\)' not optimized: 'a' is unsuitable for privatization} "" { target *-*-* } l_kernels$region }
  y = f_g (a(5)) ! { dg-line l_part[incr c_part] }
  ! { dg-optimized "assigned OpenACC gang worker vector loop parallelism" "" { target *-*-* } l_part$c_part }

  !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
  ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
  ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
  ! { dg-bogus "missed: .independent. loop in .kernels. region has not been analyzed .cf. .graphite. dumps for more information.." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
  do j = 1, N
     b(j) = y + f_w (c(j)) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
  end do
  !$acc end kernels

  !$acc kernels ! { dg-line l_kernels[incr region] }
! { dg-optimized {.map\(force_tofrom:z \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:z \[len: [0-9]+\]\[implicit\]\).} "" { target *-*-* } l_kernels$region }
! { dg-optimized {.map\(to:z \[len: [0-9]+\]\[implicit\]\). further optimized to .private\(z\).}  "" { target *-*-* } l_kernels$region }
! { dg-optimized {.map\(force_tofrom:y \[len: [0-9]+\]\[implicit\]\). optimized to .map\(to:y \[len: [0-9]+\]\[implicit\]\).}  "" { target *-*-* } l_kernels$region }
! { dg-optimized {.map\(to:y \[len: [0-9]+\]\[implicit\]\). further optimized to .private\(y\).}  "" { target *-*-* } l_kernels$region }
! { dg-missed    {\.\.\. here}  "" { target *-*-* } l_kernels$region }
! { dg-missed    {.map\(tofrom:c \[len: [0-9]+\]\[implicit\]\). not optimized: .c. is unsuitable for privatization}  "" { target *-*-* } l_kernels$region }
! { dg-missed    {.map\(tofrom:b \[len: [0-9]+\]\[implicit\]\). not optimized: .b. is unsuitable for privatization}  "" { target *-*-* } l_kernels$region }
! { dg-missed    {\.\.\. here}  "" { target *-*-* } l_kernels[expr {$region - 1}] }

  y = 3

  !$acc loop independent ! { dg-line l_loop_j[incr c_loop_j] }
  ! { dg-optimized "assigned OpenACC gang worker loop parallelism" "" { target *-*-* } l_loop_j$c_loop_j }
  ! { dg-bogus "missed: .auto. loop has not been analyzed .cf. .graphite. dumps for more information." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
  ! { dg-bogus "missed: .independent. loop in .kernels. region has not been analyzed .cf. .graphite. dumps for more information.." "TODO Inexact representation of access function in Graphite" { xfail *-*-* } l_loop_j$c_loop_j }
  do j = 1, N
     b(j) = y + f_v (c(j)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
  end do

  z = 2
  !$acc end kernels

  !$acc kernels
  !$acc end kernels  
end program main
