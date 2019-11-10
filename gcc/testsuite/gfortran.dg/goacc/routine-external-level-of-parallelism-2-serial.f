! Check invalid calls to 'external' OpenACC routines from OpenACC
! 'serial' constructs.

! { dg-additional-options "-fopt-info-optimized-omp" }

      subroutine sub
      implicit none
      integer, parameter :: n = 100
      integer :: a(n), i, j
      external :: gangr, workerr, vectorr, seqr
! { dg-note "routine 'gangr' declared here" "" { target { ! offloading_enabled } } .-1 }
! { dg-note "routine 'gangr_' declared here" "" { target offloading_enabled } .-2 }
! { dg-note "routine 'workerr' declared here" "" { target { ! offloading_enabled } } .-3 }
! { dg-note "routine 'workerr_' declared here" "" { target offloading_enabled } .-4 }
! { dg-note "routine 'vectorr' declared here" "" { target { ! offloading_enabled } } .-5 }
! { dg-note "routine 'vectorr_' declared here" "" { target offloading_enabled } .-6 }
!TODO See PR101551 for 'offloading_enabled' differences.

!$acc routine (gangr) gang
!$acc routine (workerr) worker
!$acc routine (vectorr) vector
!$acc routine (seqr) seq

!
! Test subroutine calls inside nested loops.
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
!$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
         do j = 1, n
            call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
         end do
      end do
!$acc end serial loop

!$acc serial loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
!$acc loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
         do j = 1, n
            call gangr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
         end do
      end do
!$acc end serial loop

!
! Test calls to seq routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call seqr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!
! Test calls to gang routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call gangr (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end serial loop

!
! Test calls to worker routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call workerr (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end serial loop

!
! Test calls to vector routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         call vectorr (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop
      end subroutine sub

      subroutine func
      implicit none
      integer, parameter :: n = 100
      integer :: a(n), i, j
      integer, external :: gangf, workerf, vectorf, seqf
! { dg-note "routine 'gangf' declared here" "" { target { ! offloading_enabled } } .-1 }
! { dg-note "routine 'gangf_' declared here" "" { target offloading_enabled } .-2 }
! { dg-note "routine 'workerf' declared here" "" { target { ! offloading_enabled } } .-3 }
! { dg-note "routine 'workerf_' declared here" "" { target offloading_enabled } .-4 }
! { dg-note "routine 'vectorf' declared here" "" { target { ! offloading_enabled } } .-5 }
! { dg-note "routine 'vectorf_' declared here" "" { target offloading_enabled } .-6 }
!TODO See PR101551 for 'offloading_enabled' differences.

!$acc routine (gangf) gang
!$acc routine (workerf) worker
!$acc routine (vectorf) vector
!$acc routine (seqf) seq

!
! Test subroutine calls inside nested loops.
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
!$acc loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
         do j = 1, n
            a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
         end do
      end do
!$acc end serial loop

!$acc serial loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
!$acc loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
         do j = 1, n
            a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
        end do
      end do
!$acc end serial loop

!
! Test calls to seq routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = seqf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      end do
!$acc end serial loop

!
! Test calls to gang routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC gang vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = gangf (a, n) ! { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" }
      end do
!$acc end serial loop

!
! Test calls to worker routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = workerf (a, n) ! { dg-message "optimized: assigned OpenACC worker vector loop parallelism" }
      end do
!$acc end serial loop

!
! Test calls to vector routines
!

!$acc serial loop ! { dg-message "optimized: assigned OpenACC gang worker loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop gang ! { dg-message "optimized: assigned OpenACC gang loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop worker ! { dg-message "optimized: assigned OpenACC worker loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop

!$acc serial loop vector ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
! { dg-note "containing loop here" "" { target *-*-* } .-1 }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
! { dg-error "routine call uses same OpenACC parallelism as containing loop" "" { target *-*-* } .-1 }
      end do
!$acc end serial loop

!$acc serial loop seq ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }
      do i = 1, n
         a(i) = vectorf (a, n) ! { dg-message "optimized: assigned OpenACC vector loop parallelism" }
      end do
!$acc end serial loop
      end subroutine func
