C     Test program for common block debugging.  G. Helffrich 11 July 2004.
C { dg-do compile }
C { dg-skip-if "No stabs" { mmix-*-* alpha*-*-* hppa*64*-*-* ia64-*-* *-*-vxworks* } { "*" } { "" } }
C { dg-skip-if "No stabs" {*-*-* } { "*" } { "-gstabs" } }
      common i,j
      common /label/l,m
      i = 1
      j = 2
      k = 3
      l = 4
      m = 5
      call sub
      end
      subroutine sub
      common /label/l,m
      logical first
      save n
      data first /.true./
      if (first) then
         n = 0
	 first = .false.
      endif
      n = n + 1
      l = l + 1
      return
      end

C { dg-final { scan-assembler ".stabs.*\"__BLNK__\",226" } }
C { dg-final { scan-assembler ".stabs.*\"i:V.*\",.*,0" } }
C { dg-final { scan-assembler ".stabs.*\"j:V.*\",.*,4" } }
C { dg-final { scan-assembler ".stabs.*\"__BLNK__\",228" } }
C { dg-final { scan-assembler ".stabs.*\"label_\",226" } }
C { dg-final { scan-assembler ".stabs.*\"l:V.*\",.*,0" } }
C { dg-final { scan-assembler ".stabs.*\"m:V.*\",.*,4" } }
C { dg-final { scan-assembler ".stabs.*\"label_\",228" } }
