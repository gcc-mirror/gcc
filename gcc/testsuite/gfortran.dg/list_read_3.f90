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

  if (abs(10. - a) > 1e-5) call abort
  if (abs(20. - b) > 1e-5) call abort
  if (abs(30. - c) > 1e-5) call abort
  if (abs(40. - d) > 1e-5) call abort

  a = 0
  b = 0
  c = 0
  d = 0
  write (buff,'(a)') '10.,20.,30.,40.'
  read(buff,*) a, b, c, d

  if (abs(10. - a) > 1e-5) call abort
  if (abs(20. - b) > 1e-5) call abort
  if (abs(30. - c) > 1e-5) call abort
  if (abs(40. - d) > 1e-5) call abort

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
                                                                                
  if (abs(10. - a) > 1e-5) call abort
  if (abs(20. - b) > 1e-5) call abort
  if (abs(30. - c) > 1e-5) call abort
  if (abs(40. - d) > 1e-5) call abort

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
                                                                                
  if (abs(10. - a) > 1e-5) call abort
  if (abs(-99. - b) > 1e-5) call abort
  if (abs(30. - c) > 1e-5) call abort
  if (abs(40. - d) > 1e-5) call abort

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

  if (abs(10. - a) > 1e-5) call abort
  if (abs(-20. - b) > 1e-5) call abort
  if (abs(30. - c) > 1e-5) call abort
  if (abs(-40. - d) > 1e-5) call abort

end subroutine abc
