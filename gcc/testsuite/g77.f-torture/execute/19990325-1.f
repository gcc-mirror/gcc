* test whether complex operators properly handle
* full and partial aliasing.
* (libf2c/libF77 routines used to assume no aliasing,
* then were changed to accommodate full aliasing, while
* the libg2c/libF77 versions were changed to accommodate
* both full and partial aliasing.)
*
* NOTE: this (19990325-1.f) is the double-precision version.
* See 19990325-0.f for the single-precision version.

      program doublecomplexalias
      implicit none

* Make sure non-aliased cases work.  (Catch roundoff/precision
* problems, etc., here.  Modify subroutine check if they occur.)

      call tryfull (1, 3, 5)

* Now check various combinations of aliasing.

* Full aliasing.
      call tryfull (1, 1, 5)

* Partial aliasing.
      call trypart (2, 3, 5)
      call trypart (2, 1, 5)
      call trypart (2, 5, 3)
      call trypart (2, 5, 1)

      end

      subroutine tryfull (xout, xin1, xin2)
      implicit none
      integer xout, xin1, xin2

* out, in1, and in2 are the desired indexes into the REAL array (array).

      double complex expect
      integer pwr
      integer out, in1, in2

      double precision array(6)
      double complex carray(3)
      equivalence (carray(1), array(1))

* Make sure the indexes can be accommodated by the equivalences above.

      if (mod (xout, 2) .ne. 1) call abort
      if (mod (xin1, 2) .ne. 1) call abort
      if (mod (xin2, 2) .ne. 1) call abort

* Convert the indexes into ones suitable for the COMPLEX array (carray).

      out = (xout + 1) / 2
      in1 = (xin1 + 1) / 2
      in2 = (xin2 + 1) / 2

* Check some open-coded stuff, just in case.

      call prepare1 (carray(in1))
      expect = + carray(in1)
      carray(out) = + carray(in1)
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = - carray(in1)
      carray(out) = - carray(in1)
      call check (expect, carray(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) + carray(in2)
      carray(out) = carray(in1) + carray(in2)
      call check (expect, carray(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) - carray(in2)
      carray(out) = carray(in1) - carray(in2)
      call check (expect, carray(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) * carray(in2)
      carray(out) = carray(in1) * carray(in2)
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = carray(in1) ** 2
      carray(out) = carray(in1) ** 2
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = carray(in1) ** 3
      carray(out) = carray(in1) ** 3
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = abs (carray(in1))
      array(out*2-1) = abs (carray(in1))
      array(out*2) = 0
      call check (expect, carray(out))

* Now check the stuff implemented in libF77.

      call prepare1 (carray(in1))
      expect = cos (carray(in1))
      carray(out) = cos (carray(in1))
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = exp (carray(in1))
      carray(out) = exp (carray(in1))
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = log (carray(in1))
      carray(out) = log (carray(in1))
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = sin (carray(in1))
      carray(out) = sin (carray(in1))
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = sqrt (carray(in1))
      carray(out) = sqrt (carray(in1))
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = conjg (carray(in1))
      carray(out) = conjg (carray(in1))
      call check (expect, carray(out))

      call prepare1i (carray(in1), pwr)
      expect = carray(in1) ** pwr
      carray(out) = carray(in1) ** pwr
      call check (expect, carray(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) / carray(in2)
      carray(out) = carray(in1) / carray(in2)
      call check (expect, carray(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) ** carray(in2)
      carray(out) = carray(in1) ** carray(in2)
      call check (expect, carray(out))

      call prepare1 (carray(in1))
      expect = carray(in1) ** .2
      carray(out) = carray(in1) ** .2
      call check (expect, carray(out))

      end

      subroutine trypart (xout, xin1, xin2)
      implicit none
      integer xout, xin1, xin2

* out, in1, and in2 are the desired indexes into the REAL array (array).

      double complex expect
      integer pwr
      integer out, in1, in2

      double precision array(6)
      double complex carray(3), carrayp(2)
      equivalence (carray(1), array(1))
      equivalence (carrayp(1), array(2))

* Make sure the indexes can be accommodated by the equivalences above.

      if (mod (xout, 2) .ne. 0) call abort
      if (mod (xin1, 2) .ne. 1) call abort
      if (mod (xin2, 2) .ne. 1) call abort

* Convert the indexes into ones suitable for the COMPLEX array (carray).

      out = xout / 2
      in1 = (xin1 + 1) / 2
      in2 = (xin2 + 1) / 2

* Check some open-coded stuff, just in case.

      call prepare1 (carray(in1))
      expect = + carray(in1)
      carrayp(out) = + carray(in1)
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = - carray(in1)
      carrayp(out) = - carray(in1)
      call check (expect, carrayp(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) + carray(in2)
      carrayp(out) = carray(in1) + carray(in2)
      call check (expect, carrayp(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) - carray(in2)
      carrayp(out) = carray(in1) - carray(in2)
      call check (expect, carrayp(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) * carray(in2)
      carrayp(out) = carray(in1) * carray(in2)
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = carray(in1) ** 2
      carrayp(out) = carray(in1) ** 2
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = carray(in1) ** 3
      carrayp(out) = carray(in1) ** 3
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = abs (carray(in1))
      array(out*2) = abs (carray(in1))
      array(out*2+1) = 0
      call check (expect, carrayp(out))

* Now check the stuff implemented in libF77.

      call prepare1 (carray(in1))
      expect = cos (carray(in1))
      carrayp(out) = cos (carray(in1))
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = exp (carray(in1))
      carrayp(out) = exp (carray(in1))
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = log (carray(in1))
      carrayp(out) = log (carray(in1))
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = sin (carray(in1))
      carrayp(out) = sin (carray(in1))
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = sqrt (carray(in1))
      carrayp(out) = sqrt (carray(in1))
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = conjg (carray(in1))
      carrayp(out) = conjg (carray(in1))
      call check (expect, carrayp(out))

      call prepare1i (carray(in1), pwr)
      expect = carray(in1) ** pwr
      carrayp(out) = carray(in1) ** pwr
      call check (expect, carrayp(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) / carray(in2)
      carrayp(out) = carray(in1) / carray(in2)
      call check (expect, carrayp(out))

      call prepare2 (carray(in1), carray(in2))
      expect = carray(in1) ** carray(in2)
      carrayp(out) = carray(in1) ** carray(in2)
      call check (expect, carrayp(out))

      call prepare1 (carray(in1))
      expect = carray(in1) ** .2
      carrayp(out) = carray(in1) ** .2
      call check (expect, carrayp(out))

      end

      subroutine prepare1 (in)
      implicit none
      double complex in

      in = (3.2d0, 4.2d0)

      end

      subroutine prepare1i (in, i)
      implicit none
      double complex in
      integer i

      in = (2.3d0, 2.5d0)
      i = 4

      end

      subroutine prepare2 (in1, in2)
      implicit none
      double complex in1, in2

      in1 = (1.3d0, 2.4d0)
      in2 = (3.5d0, 7.1d0)

      end

      subroutine check (expect, got)
      implicit none
      double complex expect, got

      if (dimag(expect) .ne. dimag(got)) call abort
      if (dble(expect) .ne. dble(got)) call abort

      end
