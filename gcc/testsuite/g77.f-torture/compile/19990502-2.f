C Reported by Alexander Zvyagin <zvyagin@mx.ihep.su>
C 
C Dear egcs developers,
C File g.f:
      SUBROUTINE G(IGAMS,IWRK,NADC,NCellsInY)
      INTEGER*2 IGAMS(2,NADC)
      in = 1
      do while (in.le.nadc.and.IGAMS(2,in).le.in)
      enddo
      END
C $ g++ -c g.f
C g.f:4: Internal compiler error in `expand_expr', at expr.c:6388
C Please submit a full bug report to `egcs-bugs@egcs.cygnus.com'.
C See <URL:http://egcs.cygnus.com/faq.html#bugreport> for details.
C 
C $ uname -a
C Linux gamspc5 2.2.2 #13 SMP Sat Mar 20 19:57:56 MSK 1999 i686 unknown
C $ g++ -v
C Reading specs from 
C /usr/local/lib/gcc-lib/i686-pc-linux-gnulibc1/egcs-2.93.20/specs
C gcc version egcs-2.93.20 19990427 (gcc2 ss-980929 experimental)
