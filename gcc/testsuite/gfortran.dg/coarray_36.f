! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! PR fortran/64771
!
! Contributed by Alessandro Fanfarill
!
! Reduced version of the full NAS CG benchmark
!

!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.3         !
!                                                                         !
!                                   C G                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is part of the NAS Parallel Benchmark 3.3 suite.      !
!    It is described in NAS Technical Reports 95-020 and 02-007           !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 3.3. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 3.3, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/Software/NPB/                         !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (650) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!


c---------------------------------------------------------------------
c
c Authors: M. Yarrow
c          C. Kuszmaul
c          R. F. Van der Wijngaart
c          H. Jin
c
c---------------------------------------------------------------------


c---------------------------------------------------------------------
c---------------------------------------------------------------------
      program cg
c---------------------------------------------------------------------
c---------------------------------------------------------------------
      implicit none

      integer            na, nonzer, niter
      double precision   shift, rcond
      parameter(  na=75000,
     >     nonzer=13,
     >     niter=75,
     >     shift=60.,
     >     rcond=1.0d-1 )



      integer num_proc_rows, num_proc_cols
      parameter( num_proc_rows = 2, num_proc_cols = 2)
      integer    num_procs
      parameter( num_procs = num_proc_cols * num_proc_rows )

      integer    nz
      parameter( nz = na*(nonzer+1)/num_procs*(nonzer+1)+nonzer
     >              + na*(nonzer+2+num_procs/256)/num_proc_cols )

      common / partit_size  /  naa, nzz,
     >                         npcols, nprows,
     >                         proc_col, proc_row,
     >                         firstrow,
     >                         lastrow,
     >                         firstcol,
     >                         lastcol,
     >                         exch_proc,
     >                         exch_recv_length,
     >                         send_start,
     >                         send_len
      integer                  naa, nzz,
     >                         npcols, nprows,
     >                         proc_col, proc_row,
     >                         firstrow,
     >                         lastrow,
     >                         firstcol,
     >                         lastcol,
     >                         exch_proc,
     >                         exch_recv_length,
     >                         send_start,
     >                         send_len


      common / main_int_mem /  colidx,     rowstr,
     >                         iv,         arow,     acol
      integer                  colidx(nz), rowstr(na+1),
     >                         iv(2*na+1), arow(nz), acol(nz)


c---------------------------------
c     Coarray Decalarations
c---------------------------------
      double precision         v(na+1)[0:*], aelt(nz)[0:*], a(nz)[0:*],
     >                         x(na/num_proc_rows+2)[0:*],
     >                         z(na/num_proc_rows+2)[0:*],
     >                         p(na/num_proc_rows+2)[0:*],
     >                         q(na/num_proc_rows+2)[0:*],
     >                         r(na/num_proc_rows+2)[0:*],
     >                         w(na/num_proc_rows+2)[0:*]


      common /urando/          amult, tran
      double precision         amult, tran



      integer            l2npcols
      integer            reduce_exch_proc(num_proc_cols)
      integer            reduce_send_starts(num_proc_cols)
      integer            reduce_send_lengths(num_proc_cols)
      integer            reduce_recv_lengths(num_proc_cols)
      integer            reduce_rrecv_starts(num_proc_cols)
c---------------------------------
c     Coarray Decalarations
c---------------------------------
      integer            reduce_recv_starts(num_proc_cols)[0:*]

      integer            i, j, k, it, me, nprocs, root

      double precision   zeta, randlc
      external           randlc
      double precision   rnorm
c---------------------------------
c     Coarray Decalarations
c---------------------------------
      double precision   norm_temp1(2)[0:*], norm_temp2(2)[0:*]

      double precision   t, tmax, mflops
      double precision   u(1), umax(1)
      external           timer_read
      double precision   timer_read
      character          class
      logical            verified
      double precision   zeta_verify_value, epsilon, err

c---------------------------------------------------------------------
c  Explicit interface for conj_grad, due to coarray args
c---------------------------------------------------------------------
      interface

      subroutine conj_grad ( colidx,
     >                       rowstr,
     >                       x,
     >                       z,
     >                       a,
     >                       p,
     >                       q,
     >                       r,
     >                       w,
     >                       rnorm,
     >                       l2npcols,
     >                       reduce_exch_proc,
     >                       reduce_send_starts,
     >                       reduce_send_lengths,
     >                       reduce_recv_starts,
     >                       reduce_recv_lengths,
     >                       reduce_rrecv_starts )

      common / partit_size  /  naa, nzz,
     >                         npcols, nprows,
     >                         proc_col, proc_row,
     >                         firstrow,
     >                         lastrow,
     >                         firstcol,
     >                         lastcol,
     >                         exch_proc,
     >                         exch_recv_length,
     >                         send_start,
     >                         send_len

      integer                  naa, nzz,
     >                         npcols, nprows,
     >                         proc_col, proc_row,
     >                         firstrow,
     >                         lastrow,
     >                         firstcol,
     >                         lastcol,
     >                         exch_proc,
     >                         exch_recv_length,
     >                         send_start,
     >                         send_len

      double precision   x(*),
     >                   z(*),
     >                   a(nzz)
      integer            colidx(nzz), rowstr(naa+1)

      double precision   p(*),
     >                   q(*)[0:*],
     >                   r(*)[0:*],
     >                   w(*)[0:*]        ! used as work temporary

      integer   l2npcols
      integer   reduce_exch_proc(l2npcols)
      integer   reduce_send_starts(l2npcols)
      integer   reduce_send_lengths(l2npcols)
      integer   reduce_recv_starts(l2npcols)[0:*]
      integer   reduce_recv_lengths(l2npcols)
      integer   reduce_rrecv_starts(l2npcols)

      double precision   rnorm

      end subroutine

      end interface

c---------------------------------------------------------------------
c  The call to the conjugate gradient routine:
c---------------------------------------------------------------------
         call conj_grad ( colidx,
     >                    rowstr,
     >                    x,
     >                    z,
     >                    a,
     >                    p,
     >                    q,
     >                    r,
     >                    w,
     >                    rnorm,
     >                    l2npcols,
     >                    reduce_exch_proc,
     >                    reduce_send_starts,
     >                    reduce_send_lengths,
     >                    reduce_recv_starts,
     >                    reduce_recv_lengths,
     >                    reduce_rrecv_starts ) 


      sync all

      end                              ! end main

c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine conj_grad ( colidx,
     >                       rowstr,
     >                       x,
     >                       z,
     >                       a,
     >                       p,
     >                       q,
     >                       r,
     >                       w,
     >                       rnorm,
     >                       l2npcols,
     >                       reduce_exch_proc,
     >                       reduce_send_starts,
     >                       reduce_send_lengths,
     >                       reduce_recv_starts,
     >                       reduce_recv_lengths,
     >                       reduce_rrecv_starts )
c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c  Floaging point arrays here are named as in NPB1 spec discussion of
c  CG algorithm
c---------------------------------------------------------------------

      implicit none

c      include 'cafnpb.h'

      common / partit_size  /  naa, nzz,
     >                         npcols, nprows,
     >                         proc_col, proc_row,
     >                         firstrow,
     >                         lastrow,
     >                         firstcol,
     >                         lastcol,
     >                         exch_proc,
     >                         exch_recv_length,
     >                         send_start,
     >                         send_len
      integer                  naa, nzz,
     >                         npcols, nprows,
     >                         proc_col, proc_row,
     >                         firstrow,
     >                         lastrow,
     >                         firstcol,
     >                         lastcol,
     >                         exch_proc,
     >                         exch_recv_length,
     >                         send_start,
     >                         send_len



      double precision   x(*),
     >                   z(*),
     >                   a(nzz)
      integer            colidx(nzz), rowstr(naa+1)

      double precision   p(*),
     >                   q(*)[0:*],
     >                   r(*)[0:*],
     >                   w(*)[0:*]        ! used as work temporary

      integer   l2npcols
      integer   reduce_exch_proc(l2npcols)
      integer   reduce_send_starts(l2npcols)
      integer   reduce_send_lengths(l2npcols)
      integer   reduce_recv_starts(l2npcols)[0:*]
      integer   reduce_recv_lengths(l2npcols)
      integer   reduce_rrecv_starts(l2npcols)

      integer   recv_start_idx, recv_end_idx, send_start_idx,
     >          send_end_idx, recv_length

      integer   i, j, k, ierr
      integer   cgit, cgitmax

      double precision, save :: d[0:*], rho[0:*]
      double precision   sum, rho0, alpha, beta, rnorm

      external         timer_read
      double precision timer_read

      data      cgitmax / 25 /


      return
      end                       ! end of routine conj_grad

