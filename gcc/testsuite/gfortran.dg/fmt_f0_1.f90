! { dg-do run }
! PR39304 write of  0.0 with F0.3 gives  **
! PR47567 Small absolute values.
! Test case developed from case provided by reporter.
 REAL :: x
 CHARACTER(80) :: str
 x = 0.0
 write (str,'(f0.0)') x
 if (str.ne."0.") STOP 1
 write (str,'(f0.1)') x
 if (str.ne.".0") STOP 2
 write (str,'(f0.2)') x
 if (str.ne.".00") STOP 3
 write (str,'(f0.3)') x
 if (str.ne.".000") STOP 4
 write (str,'(f0.4)') x
 if (str.ne.".0000") STOP 5
 write (str,'(F0.0)') 0.0
 if (str.ne."0.") STOP 6
 write (str,'(F0.0)') 0.001
 if (str.ne."0.") STOP 7
 write (str,'(F0.0)') 0.01
 if (str.ne."0.") STOP 8
 write (str,'(F0.0)') 0.1
 if (str.ne."0.") STOP 9
 write (str,'(F1.0)') -0.0
 if (str.ne."*") STOP 10
 write (str,'(F1.0)') 0.001
 if (str.ne."*") STOP 11
 write (str,'(F1.0)') 0.01
 if (str.ne."*") STOP 12
 write (str,'(F1.0)') 0.1
 if (str.ne."*") STOP 13
 write (str,'(F2.0)') -0.001
 if (str.ne."**") STOP 14
 write (str,'(F2.0)') -0.01
 if (str.ne."**") STOP 15
 write (str,'(F2.0)') -0.1
 if (str.ne."**") STOP 16
 write (str,'(F0.2)') 0.0
 if (str.ne.".00") STOP 17
 write (str,'(F0.0)') -0.0
 if (str.ne."-0.") STOP 18
 write (str,'(F0.1)') -0.0
 if (str.ne."-.0") STOP 19
 write (str,'(F0.2)') -0.0
 if (str.ne."-.00") STOP 20
 write (str,'(F0.3)') -0.0
 if (str.ne."-.000") STOP 21
 write (str,'(F3.0)') -0.0
 if (str.ne."-0.") STOP 22
 write (str,'(F2.0)') -0.0
 if (str.ne."**") STOP 23
 write (str,'(F1.0)') -0.0
 if (str.ne."*") STOP 24
 write (str,'(F0.1)') -0.0
 if (str.ne."-.0") STOP 25
 write (str,'(F3.1)') -0.0
 if (str.ne."-.0") STOP 26
 write (str,'(F2.1)') -0.0
 if (str.ne."**") STOP 27
 write (str,'(F1.1)') -0.0
 if (str.ne."*") STOP 28
 END  
