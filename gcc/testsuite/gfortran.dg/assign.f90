! { dg-do run }
! Program to test ASSIGNing a label to common variable. PR18827.
      program test
      integer i
      common i
      assign 2000 to i  ! { dg-warning "Obsolete: ASSIGN statement" }
2000  continue
      end
