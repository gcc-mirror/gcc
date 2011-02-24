! { dg-do run }
! PR39304 write of  0.0 with F0.3 gives  **
! PR47567 Small absolute values.
! Test case developed from case provided by reporter.
 REAL :: x
 CHARACTER(80) :: str
 x = 0.0
 write (str,'(f0.0)') x
 if (str.ne."0.") call abort
 write (str,'(f0.1)') x
 if (str.ne.".0") call abort
 write (str,'(f0.2)') x
 if (str.ne.".00") call abort
 write (str,'(f0.3)') x
 if (str.ne.".000") call abort
 write (str,'(f0.4)') x
 if (str.ne.".0000") call abort
 write (str,'(F0.0)') 0.0
 if (str.ne."0.") call abort
 write (str,'(F0.0)') 0.001
 if (str.ne."0.") call abort
 write (str,'(F0.0)') 0.01
 if (str.ne."0.") call abort
 write (str,'(F0.0)') 0.1
 if (str.ne."0.") call abort
 write (str,'(F1.0)') -0.0
 if (str.ne."*") call abort
 write (str,'(F1.0)') 0.001
 if (str.ne."*") call abort
 write (str,'(F1.0)') 0.01
 if (str.ne."*") call abort
 write (str,'(F1.0)') 0.1
 if (str.ne."*") call abort
 write (str,'(F2.0)') -0.001
 if (str.ne."**") call abort
 write (str,'(F2.0)') -0.01
 if (str.ne."**") call abort
 write (str,'(F2.0)') -0.1
 if (str.ne."**") call abort
 write (str,'(F0.2)') 0.0
 if (str.ne.".00") call abort
 write (str,'(F0.0)') -0.0
 if (str.ne."-0.") call abort
 write (str,'(F0.1)') -0.0
 if (str.ne."-.0") call abort
 write (str,'(F0.2)') -0.0
 if (str.ne."-.00") call abort
 write (str,'(F0.3)') -0.0
 if (str.ne."-.000") call abort
 write (str,'(F3.0)') -0.0
 if (str.ne."-0.") call abort
 write (str,'(F2.0)') -0.0
 if (str.ne."**") call abort
 write (str,'(F1.0)') -0.0
 if (str.ne."*") call abort
 write (str,'(F0.1)') -0.0
 if (str.ne."-.0") call abort
 write (str,'(F3.1)') -0.0
 if (str.ne."-.0") call abort
 write (str,'(F2.1)') -0.0
 if (str.ne."**") call abort
 write (str,'(F1.1)') -0.0
 if (str.ne."*") call abort
 END  
