C Test compiler flags: -fcase-preserve
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fcase-preserve" }
      i = 3
      I = 4
      if ( i .ne. 3 ) call abort
      end
