C Test Fortran 77 X descriptor 
C      (ANSI X3.9-1978 Section 13.5.3.2)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run } 
C ( dg-output "^" }
      write(*,'(I1,1X,I1,2X,I1)') 1,2,3    ! { dg-output "1 2  3(\n|\r\n|\r)" }
C Section 13.5.3 explains why there are no trailing blanks
      write(*,'(I1,1X,I1,2X,I1,3X)') 1,2,3 ! { dg-output "1 2  3(\n|\r\n|\r)" }
C { dg-output "\$" {xfail *-*-*} } gfortran PR 16435
      end
