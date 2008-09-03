C     Test program for common block debugging.  G. Helffrich 11 July 2004.
C { dg-do compile }
C { dg-skip-if "DWARF-2 only" { "*-*-*" } { "*" } { "-gdwarf-2" } }
C { dg-options "-dA" }
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

C { dg-final { scan-assembler "DIE\[^\n\]*DW_TAG_common_block" } }
C { dg-final { scan-assembler "(DW_AT_name: \"__BLNK__\"|\"__BLNK__\[^\n\]*\"\[^\n\]*DW_AT_name)" } }
C { dg-final { scan-assembler "DIE\[^\n\]*DW_TAG_variable" } }
C { dg-final { scan-assembler "\"i\[^\n\]*\"\[^\n\]*DW_AT_name" } }
C { dg-final { scan-assembler "\"j\[^\n\]*\"\[^\n\]*DW_AT_name" } }
C { dg-final { scan-assembler "DIE\[^\n\]*DW_TAG_common_block" } }
C { dg-final { scan-assembler "(DW_AT_name: \"label\"|\"label\[^\n\]*\"\[^\n\]*DW_AT_name)" } }
C { dg-final { scan-assembler "DIE\[^\n\]*DW_TAG_variable" } }
C { dg-final { scan-assembler "\"l\[^\n\]*\"\[^\n\]*DW_AT_name" } }
C { dg-final { scan-assembler "\"m\[^\n\]*\"\[^\n\]*DW_AT_name" } }
