! Test OpenACC 'kernels' construct decomposition with "decompose-parloops"
! handling

! { dg-additional-options "--param openacc-kernels=decompose-parloops" }
! { dg-additional-options "-fopt-info-optimized-omp" }
! { dg-additional-options "-Wopenacc-parallelism" }
! { dg-additional-options "-O2" } for "parloops".

! See also "../../c-c++-common/goacc/kernels-decompose-1.c".

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

  !$acc kernels
  x = 0
  y = 0
  y_l = x < 10
  z = x
  x = x + 1
  !$acc end kernels

  !$acc kernels
  do i = 1, N
     ! { dg-optimized "assigned OpenACC gang loop parallelism" "" { target *-*-* } .-1 }
     a(i) = 0
  end do
  !$acc end kernels

  !$acc kernels loop ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc kernels
  !$acc loop ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, N
     b(i) = a(N - i + 1)
  end do

  !$acc loop ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, N
     c(i) = a(i) * b(i)
  end do

  a(z) = 0

  !$acc loop ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do i = 1, N
     c(i) = c(i) + a(i)
  end do

  !$acc loop seq ! { dg-optimized "assigned OpenACC seq loop parallelism" }
  do i = 1 + 1, N
     c(i) = c(i) + c(i - 1)
  end do
  !$acc end kernels

  !$acc kernels ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
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

  !TODO Should the following turn into "gang-single" instead of "parloops"?
  !TODO The problem is that the first STMT is "if (y <= 4) goto <D.2547>; else goto <D.2548>;", thus "parloops".
  if (y < 5) then
     !$acc loop independent
     do j = 1, N
        b(j) = f_w (c(j))
     end do
  end if
  !$acc end kernels

  !$acc kernels  ! { dg-warning "region contains gang partitioned code but is not gang partitioned" }
  y = f_g (a(5)) ! { dg-optimized "assigned OpenACC gang worker vector loop parallelism" }

  !$acc loop independent ! { dg-optimized "assigned OpenACC gang loop parallelism" }
  do j = 1, N
     b(j) = y + f_w (c(j)) ! { dg-optimized "assigned OpenACC worker vector loop parallelism" }
  end do
  !$acc end kernels

  !$acc kernels
  y = 3

  !$acc loop independent ! { dg-optimized "assigned OpenACC gang worker loop parallelism" }
  do j = 1, N
     b(j) = y + f_v (c(j)) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
  end do

  z = 2
  !$acc end kernels

  !$acc kernels
  !$acc end kernels  
end program main
