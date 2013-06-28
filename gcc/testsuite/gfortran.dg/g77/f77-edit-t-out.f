C Test Fortran 77 T edit descriptor 
C      (ANSI X3.9-1978 Section 13.5.3.2)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-output "^" }
      write(*,'(I4,T8,I1)')     1234,8 ! { dg-output "1234   8(\n|\r\n|\r)" }
      write(*,'(I4,TR3,I1)')    1234,8 ! { dg-output "1234   8(\n|\r\n|\r)" }
      write(*,'(I4,5X,TL2,I1)') 1234,8 ! { dg-output "1234   8(\n|\r\n|\r)" }
C { dg-output "\$" }
      end
