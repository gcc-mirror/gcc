C Test compiler flags: -fno-underscoring
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do compile }
C { dg-options "-fno-underscoring" }
      call aaabbbccc
      end
C { dg-final { scan-assembler-not "aaabbbccc_" } }
