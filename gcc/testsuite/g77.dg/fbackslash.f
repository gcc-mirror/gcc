C Test compiler flags: -fbackslash
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fbackslash" }
      if ( len('A\nB') .ne. 3 ) call abort
      end
