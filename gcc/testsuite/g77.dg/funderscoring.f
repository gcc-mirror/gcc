C Test compiler flags: -funderscoring
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do compile }
C { dg-options "-funderscoring" }
      call aaabbbccc
      end
C { dg-final { scan-assembler "aaabbbccc_" } }
