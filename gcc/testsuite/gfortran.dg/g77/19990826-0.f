c { dg-do run }
* From: niles@fan745.gsfc.nasa.gov
* To: fortran@gnu.org
* Cc: niles@fan745.gsfc.nasa.gov
* Subject: problem with DNINT() on Linux/Alpha.
* Date: Sun, 06 Jun 1999 16:39:35 -0400
* X-UIDL: 6aa9208d7bda8b6182a095dfd37016b7

      IF (DNINT(0.0D0) .NE. 0.) STOP 1
      STOP
      END

* Result on Linux/i386: " 0."  (and every other computer!)
* Result on Linux/alpha: " 3.6028797E+16"

* It seems to work fine if I change it to the generic NINT().  Probably
* a name pollution problem in the new C library, but it seems bad. no?

* Thanks,
* Rick Niles.
