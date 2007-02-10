C { dg-do run }
C { dg-options "-O0 -ffloat-store" }
C Tests fix for PR14994 - SECNDS intrinsic not supported.
C
C Contributed by Paul Thomas  <pault@gcc.gnu.org>
C
      character*20 dum1, dum2, dum3
      real t1, t1a, t2, t2a
      real dat1, dat2
      integer i, j, values(8)
      t1 = secnds (0.0)
      call date_and_time (dum1, dum2, dum3, values)
      t1a = secnds (0.0)
      dat1 = 0.001*real (values(8)) + real (values(7)) +
     &        60.0*real (values(6)) + 3600.0* real (values(5))
      if (((dat1 - t1) < 0.) .or. ((dat1 - t1) > (t1a - t1))) call abort ()
      do j=1,10000
        do i=1,10000
        end do
      end do
      t2a = secnds (t1a)
      call date_and_time (dum1, dum2, dum3, values)
      t2 = secnds (t1)
      dat2 = 0.001*real (values(8)) + real (values(7)) +
     &        60.0*real (values(6)) + 3600.0* real (values(5))
      if (((dat2 - dat1) < t2a) .or. ((dat2 - dat1) > t2)) call abort ()
      end
