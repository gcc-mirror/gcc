C Test compiler flags: -fno-vxt
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fno-vxt" }
      i = 0
     !1
      if ( i .ne. 0 ) call exit
      call abort
      END
