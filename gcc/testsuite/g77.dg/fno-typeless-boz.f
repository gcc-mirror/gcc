C Test compiler flags: -fno-typeless-boz
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fno-typeless-boz" }
      equivalence (i,r)
      r = Z'ABCD1234'
      j = Z'ABCD1234'
      if ( j .eq. i ) call abort
      end
