C Test case for PR fortran/3743
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do link }
C { dg-options "-fcase-preserve -fintrin-case-upper" }
      integer   i
      i = BIT_SIZE(i)
      end
