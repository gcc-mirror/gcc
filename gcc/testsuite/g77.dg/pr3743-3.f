c Test case for PR fortran/3743
c Origin: David Billinghurst <David.Billinghurst@riotinto.com>
c
c { dg-do link }
c { dg-options "-fcase-preserve -fintrin-case-lower" }
      integer   i
      i = bit_size(i)
      end
