C Test Fortran 77 S, SS and SP edit descriptors 
C      (ANSI X3.9-1978 Section 13.5.6)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-output "^" }
 10   format(SP,I3,1X,SS,I3)
 20   format(SP,I3,1X,SS,I3,SP,I3)
 30   format(SP,I3,1X,SS,I3,S,I3)
 40   format(SP,I3)
 50   format(SP,I2)
      write(*,10) 10, 20      ! { dg-output "\\+10  20(\n|\r\n|\r)" }
      write(*,20) 10, 20, 30  ! { dg-output "\\+10  20\\+30(\n|\r\n|\r)" }
      write(*,30) 10, 20, 30  ! { dg-output "\\+10  20 30(\n|\r\n|\r)" } 
      write(*,40) 0           ! { dg-output " \\+0(\n|\r\n|\r)" }
C 15.5.9 - Note 5: When SP editing is in effect, the plus sign is not optional
      write(*,50) 11          ! { dg-output "\\*\\*(\n|\r\n|\r)" }
C { dg-output "\$" }
      end
