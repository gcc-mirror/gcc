c { dg-do compile }
* Fixed by JCB 1998-07-25 change to stc.c.

* Date: Thu, 11 Jun 1998 22:35:20 -0500
* From: Ian A Watson <WATSON_IAN_A@lilly.com>
* Subject: crash
* 
      CaLL foo(W)
      END
      SUBROUTINE foo(W)
      yy(I)=A(I)Q(X) ! { dg-error "Unclassifiable statement" "" }
c { dg-error "end of file" "end of file" { target *-*-* } 0 }
