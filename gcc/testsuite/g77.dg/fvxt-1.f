C Test compiler flags: -fvxt
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fvxt" }
      i = 0
     !1
      if ( i .eq. 0 ) call exit
      call abort
      END
