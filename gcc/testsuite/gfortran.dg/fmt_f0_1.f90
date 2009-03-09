! { dg-do run )
! PR39304 write of  0.0 with F0.3 gives  **
! Test case developed from case provided by reporter.
 REAL :: x
 CHARACTER(80) :: str
 x = 0.0
 write (str,'(f0.0)') x
 if (str.ne."0.") call abort
 write (str,'(f0.1)') x
 if (str.ne."0.0") call abort
 write (str,'(f0.2)') x
 if (str.ne."0.00") call abort
 write (str,'(f0.3)') x
 if (str.ne."0.000") call abort
 write (str,'(f0.4)') x
 if (str.ne."0.0000") call abort
 END  
