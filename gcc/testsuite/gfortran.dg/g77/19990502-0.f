c { dg-do compile }
* Mailing-List: contact egcs-bugs-help@egcs.cygnus.com; run by ezmlm
* Precedence: bulk
* Sender: owner-egcs-bugs@egcs.cygnus.com
* From: Norbert Conrad <Norbert.Conrad@hrz.uni-giessen.de>
* Subject: egcs g77 19990524pre Internal compiler error in `print_operand'
* To: egcs-bugs@egcs.cygnus.com
* Date: Mon, 31 May 1999 11:46:52 +0200 (CET)
* Content-Type: text/plain; charset=US-ASCII
* X-UIDL: 9a00095a5fe4d774b7223de071157374
* 
* Hi,
* 
* I ./configure --prefix=/opt and bootstrapped egcs g77 snapshot 19990524
* on an i686-pc-linux-gnu. The program below gives an internal compiler error.
* 
* 
* Script started on Mon May 31 11:30:01 1999
* lx{g010}:/tmp>/opt/bin/g77 -v -O3 -malign-double -c e3.f
* g77 version gcc-2.95 19990524 (prerelease) (from FSF-g77 version 0.5.24-19990515)
* Reading specs from /opt/lib/gcc-lib/i686-pc-linux-gnu/gcc-2.95/specs
* gcc version gcc-2.95 19990524 (prerelease)
*  /opt/lib/gcc-lib/i686-pc-linux-gnu/gcc-2.95/f771 e3.f -quiet -dumpbase e3.f -malign-double -O3 -version -fversion -o /tmp/ccQgeaaa.s
* GNU F77 version gcc-2.95 19990524 (prerelease) (i686-pc-linux-gnu) compiled by GNU C version gcc-2.95 19990524 (prerelease).
* GNU Fortran Front End version 0.5.24-19990515
* e3.f:25: Internal compiler error in `print_operand', at ./config/i386/i386.c:3405
* Please submit a full bug report to `egcs-bugs@egcs.cygnus.com'.
* See <URL:http://egcs.cygnus.com/faq.html#bugreport> for details.
* lx{g010}:/tmp>cat e3.f
      SUBROUTINE DLASQ2( QQ, EE,  TOL2, SMALL2 )
      DOUBLE PRECISION   SMALL2, TOL2
      DOUBLE PRECISION   EE( * ), QQ( * )
      INTEGER            ICONV,  N, OFF
      DOUBLE PRECISION   QEMAX, XINF
      EXTERNAL           DLASQ3
      INTRINSIC          MAX, SQRT
      XINF = 0.0D0
      ICONV = 0
      IF( EE( N ).LE.MAX( QQ( N ), XINF, SMALL2 )*TOL2 ) THEN
      END IF
      IF( EE( N-2 ).LE.MAX( XINF, SMALL2,
     $    ( QQ( N ) / ( QQ( N )+EE( N-1 ) ) )* QQ( N-1 ))*TOL2 ) THEN
         QEMAX = MAX( QQ( N ), QQ( N-1 ), EE( N-1 ) )
      END IF
      IF( N.EQ.0 ) THEN
         IF( OFF.EQ.0 ) THEN
            RETURN
         ELSE
            XINF =0.0D0 
         END IF
      ELSE IF( N.EQ.2 ) THEN
      END IF
      CALL DLASQ3(ICONV)
      END
* lx{g010}:/tmp>exit
* 
* Script done on Mon May 31 11:30:23 1999
* 
* Best regards,
* 
* Norbert.
* -- 
* Norbert Conrad                             phone: ++49 641 9913021
* Hochschulrechenzentrum                     email: conrad@hrz.uni-giessen.de
* Heinrich-Buff-Ring 44
* 35392 Giessen
* Germany
