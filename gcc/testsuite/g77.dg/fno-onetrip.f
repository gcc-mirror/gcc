C Test compiler flags: -fno-onetrip
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fno-onetrip -w" }
      do i = 1, 0
         call abort
      end do
      end
