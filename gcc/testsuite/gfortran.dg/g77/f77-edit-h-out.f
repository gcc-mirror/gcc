C Test Fortran 77 H edit descriptor
C      (ANSI X3.9-1978 Section 13.5.2)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-output "^" }
C { dg-options "-std=legacy"
 10   format(1H1) ! { dg-warning "H format specifier" }
 20   format(6H     6) ! { dg-warning "H format specifier" }
      write(*,10)        ! { dg-output "1(\r*\n+)" }
      write(*,20)        ! { dg-output "     6(\r*\n+)" }
      write(*,'(16H''apostrophe'' fun)') ! { dg-warning "H format specifier" }
       ! { dg-output "'apostrophe' fun(\r*\n+)" }
C { dg-output "\$" }
      end
