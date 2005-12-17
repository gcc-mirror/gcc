C { dg-do run }
C { dg-options "-O0" }
C Tests fix for PR14994 - SECNDS intrinsic not supported.
C Note1: The test uses +/-20ms accuracy in the check that
C date_and_time and secnds give the same values.
C
C Contributed by Paul Thomas  <pault@gcc.gnu.org>
C
      character*20 dum1, dum2, dum3
      real t1, t2
      real dat1, dat2
      real dt
      integer i, j, values(8)
      dt = 40e-3
      t1 = secnds (0.0)
      call date_and_time (dum1, dum2, dum3, values)
      dat1 = 0.001*real (values(8)) + real (values(7)) +
     &        60.0*real (values(6)) + 3600.0* real (values(5))
      if (int ((dat1 - t1 + dt * 0.5) / dt) .ne. 0) call abort ()
      do j=1,10000
        do i=1,10000
        end do
      end do
      call date_and_time (dum1, dum2, dum3, values)
      dat2 = 0.001*real (values(8)) + real (values(7)) +
     &        60.0*real (values(6)) + 3600.0* real (values(5))
      t2 = secnds (t1)
      if (int ((dat1-dat2 + t2 + dt * 0.5) / dt) .ne. 0.0) call abort ()
      end
