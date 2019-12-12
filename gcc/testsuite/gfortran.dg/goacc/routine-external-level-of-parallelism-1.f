! Check valid calls to 'external' OpenACC routines.

! { dg-additional-options "-fopt-info-optimized-omp" }

      subroutine sub
      implicit none
      integer, parameter :: n = 100
      integer :: a(n), i, j
      external :: gangr, workerr, vectorr, seqr
!$acc routine (gangr) gang
!$acc routine (workerr) worker
!$acc routine (vectorr) vector
!$acc routine (seqr) seq

!
! Test subroutine calls inside nested loops.
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
!$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
         do j = 1, n
            call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
         end do
      end do
!$acc end parallel loop

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
!$acc loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
         do j = 1, n
            call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
         end do
      end do
!$acc end parallel loop

!
! Test calls to seq routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!
! Test calls to gang routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end parallel loop

!
! Test calls to worker routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!
! Test calls to vector routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop
      end subroutine sub

      subroutine func
      implicit none
      integer, parameter :: n = 100
      integer :: a(n), i, j
      integer, external :: gangf, workerf, vectorf, seqf
!$acc routine (gangf) gang
!$acc routine (workerf) worker
!$acc routine (vectorf) vector
!$acc routine (seqf) seq

!
! Test subroutine calls inside nested loops.
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
!$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
         do j = 1, n
            a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
         end do
      end do
!$acc end parallel loop

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
!$acc loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
         do j = 1, n
            a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
         end do
      end do
!$acc end parallel loop

!
! Test calls to seq routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!
! Test calls to gang routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end parallel loop

!
! Test calls to worker routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end parallel loop

!
! Test calls to vector routines
!

!$acc parallel loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end parallel loop

!$acc parallel loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end parallel loop
      end subroutine func
