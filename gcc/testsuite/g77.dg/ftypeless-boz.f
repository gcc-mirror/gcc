C Test compiler flags: -ftypeless-boz
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-ftypeless-boz" }
      equivalence (i,r)
      r = Z'ABCD1234'
      j = Z'ABCD1234'
      if ( j .ne. i ) call abort
      end
