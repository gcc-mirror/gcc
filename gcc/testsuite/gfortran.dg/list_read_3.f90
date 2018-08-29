! { dg-do run }
! Program to test reading in a list of integer values into REAL variables.
! The comma separator was not handled correctly.
!
program fg

  character(len=80) buff
  logical debug

  debug = .FALSE.
  a = 0
  b = 0
  c = 0
  d = 0
  write (buff,'(a)') '10,20,30,40'
  read(buff,*) a, b, c, d

  if (debug) then
    print*,buff
    print*,a, b, c, d
  end if

  if (abs(10. - a) > 1e-5) STOP 1
  if (abs(20. - b) > 1e-5) STOP 2
  if (abs(30. - c) > 1e-5) STOP 3
  if (abs(40. - d) > 1e-5) STOP 4

  a = 0
  b = 0
  c = 0
  d = 0
  write (buff,'(a)') '10.,20.,30.,40.'
  read(buff,*) a, b, c, d

  if (abs(10. - a) > 1e-5) STOP 5
  if (abs(20. - b) > 1e-5) STOP 6
  if (abs(30. - c) > 1e-5) STOP 7
  if (abs(40. - d) > 1e-5) STOP 8

  if (debug) then
    print*,buff
    print*,a, b, c, d
  end if 

  a = 0
  b = 0
  c = 0
  d = 0
  write (buff,'(a)') '10.0,20.0,30.0,40.0'
  read(buff,*) a, b, c, d
                                                                                
  if (abs(10. - a) > 1e-5) STOP 9
  if (abs(20. - b) > 1e-5) STOP 10
  if (abs(30. - c) > 1e-5) STOP 11
  if (abs(40. - d) > 1e-5) STOP 12

  if (debug) then 
    print*,buff
    print*,a, b, c, d
  end if
                                                                              

  a = 0
  b = -99 
  c = 0
  d = 0
  write (buff,'(a)') '10.0,,30.0,40.0'
  read(buff,*) a, b, c, d
                                                                                
  if (abs(10. - a) > 1e-5) STOP 13
  if (abs(-99. - b) > 1e-5) STOP 14
  if (abs(30. - c) > 1e-5) STOP 15
  if (abs(40. - d) > 1e-5) STOP 16

  if (debug) then
    print*,buff
    print*,a, b, c, d
  end if
                                                                                

   call abc

end program

subroutine abc

  character(len=80) buff

  a = 0
  b = 0
  c = 0
  d = 0
  write (buff,'(a)') '10,-20,30,-40'
  read(buff,*) a, b, c, d

  if (abs(10. - a) > 1e-5) STOP 17
  if (abs(-20. - b) > 1e-5) STOP 18
  if (abs(30. - c) > 1e-5) STOP 19
  if (abs(-40. - d) > 1e-5) STOP 20

end subroutine abc
