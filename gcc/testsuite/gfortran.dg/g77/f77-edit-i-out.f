C Test Fortran 77 I edit descriptor for output
C      (ANSI X3.9-1978 Section 13.5.9.1)
C
C Origin: David Billinghurst <David.Billinghurst@riotinto.com>
C
C { dg-do run }
C { dg-output "^" }

      write(*,'(I1)')    1  ! { dg-output "1(\n|\r\n|\r)" }
      write(*,'(I1)')   -1  ! { dg-output "\\*(\n|\r\n|\r)" }
      write(*,'(I2)')    2  ! { dg-output " 2(\n|\r\n|\r)" }
      write(*,'(I2)')   -2  ! { dg-output "-2(\n|\r\n|\r)" }
      write(*,'(I3)')    3  ! { dg-output "  3(\n|\r\n|\r)" }
      write(*,'(I3)')   -3  ! { dg-output " -3(\n|\r\n|\r)" }

      write(*,'(I2.0)')  0  ! { dg-output "  (\n|\r\n|\r)" }
      write(*,'(I1.1)')  4  ! { dg-output "4(\n|\r\n|\r)" }
      write(*,'(I1.1)') -4  ! { dg-output "\\*(\n|\r\n|\r)" }
      write(*,'(I2.1)')  5  ! { dg-output " 5(\n|\r\n|\r)" }
      write(*,'(I2.1)') -5  ! { dg-output "-5(\n|\r\n|\r)" }
      write(*,'(I2.2)')  6  ! { dg-output "06(\n|\r\n|\r)" }
      write(*,'(I2.2)') -6  ! { dg-output "\\*\\*(\n|\r\n|\r)" }
      write(*,'(I3.2)')  7  ! { dg-output " 07(\n|\r\n|\r)" }
      write(*,'(I3.2)') -7  ! { dg-output "-07(\n|\r\n|\r)" }

      end
