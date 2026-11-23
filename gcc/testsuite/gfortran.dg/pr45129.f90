! { dg-do compile }
! { dg-options "-Wall" }
!
! PR 45129

program p
  real :: x = 1.0
  character(20) :: s = "1.0"
100 format (F2.3)   ! { dg-warning "Field width of the F edit descriptor" }
200 format (E5.3)   ! { dg-warning "Field width of the E edit descriptor" }
300 format (G4.2)   ! { dg-warning "Field width of the G edit descriptor" }
400 format (F1.0)
500 format (F4.2)
  read (s, 400) x
  read (s, 100) x
  write (s, 100) x
  write (s, 100) x
  print 200, x
  write (s, 300) x
  write (s, 500) x
  read (s, '(F1.0)') x
  write (s, '(F1.0)') x      ! { dg-warning "Field width of the F edit descriptor" }
  write (s, '(F2.0)') x
  write (s, '(F2.2)') x      ! { dg-warning "Field width of the F edit descriptor" }
  write (s, '(F3.2)') x
  write (s, '(E6.2)') x      ! { dg-warning "Field width of the E edit descriptor" }
  write (s, '(E7.2)') x
  write (s, '(E7.2E3)') x    ! { dg-warning "Field width of the E edit descriptor" }
  write (s, '(E8.2E3)') x
  write (s, '(G5.2)') x      ! { dg-warning "Field width of the G edit descriptor" }
  write (s, '(G6.2)') x
  write (s, '(E4.1E0)') x    ! { dg-warning "Field width of the E edit descriptor" }
  write (s, '(E5.1E0)') x
  write (s, '(ES4.1E0)') x   ! { dg-warning "Field width of the ES edit descriptor" }
  write (s, '(ES5.1E0)') x   ! { dg-warning "Field width of the ES edit descriptor" }
  write (s, '(ES6.1E0)') x
  write (s, '(ES7.2)') x     ! { dg-warning "Field width of the ES edit descriptor" }
  write (s, '(ES8.2)') x
  write (s, '(EN5.1E0)') x   ! { dg-warning "Field width of the EN edit descriptor" }
  write (s, '(EN6.1E0)') x
  write (s, '(D6.2)') x      ! { dg-warning "Field width of the D edit descriptor" }
  write (s, '(D7.2)') x
  write (s, '(G4.1E0)') x    ! { dg-warning "Field width of the G edit descriptor" }
  write (s, '(G5.1E0)') x
end program
