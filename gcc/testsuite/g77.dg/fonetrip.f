C Test compiler flags: -fonetrip
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-options "-fonetrip -w" }
      do i = 1, 0
         call exit
      end do
      call abort
      end
