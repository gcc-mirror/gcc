C Test compiler flags: -fno-backslash
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fno-backslash" }
      if ( len('A\nB') .ne. 4 ) call abort
      end
