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

C { dg-final { scan-assembler "(DIE.*DW_TAG_common_block)" } }
C { dg-final { scan-assembler "DW_AT_name: \"__BLNK__\"" } }
C { dg-final { scan-assembler "(DIE.*DW_TAG_member)" } }
C { dg-final { scan-assembler "\"i.*\".*DW_AT_name" } }
C { dg-final { scan-assembler "\"j.*\".*DW_AT_name" } }
C { dg-final { scan-assembler "(DIE.*DW_TAG_common_block)" } }
C { dg-final { scan-assembler "DW_AT_name: \"label\"" } }
C { dg-final { scan-assembler "(DIE.*DW_TAG_member)" } }
C { dg-final { scan-assembler "\"l.*\".*DW_AT_name" } }
C { dg-final { scan-assembler "\"m.*\".*DW_AT_name" } }
