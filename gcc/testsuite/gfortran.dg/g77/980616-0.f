c { dg-do compile }
* Fixed by 1998-07-11 equiv.c change.
* ../../gcc/f/equiv.c:666: failed assertion `ffebld_op (subscript) == FFEBLD_opCONTER'

* Date: Mon, 15 Jun 1998 21:54:32 -0500
* From: Ian A Watson <WATSON_IAN_A@lilly.com>
* Subject: Mangler Crash
      EQUIVALENCE(I,glerf(P)) ! { dg-error "cannot appear"  "cannot appear" }
      COMMON /foo/ glerf(3)
c { dg-error "end of file" "end of file" { target *-*-* } 0 }
