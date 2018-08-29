! { dg-do  run }
! PR fortran/66709
! Check that parameter formats are handled correctly.
! Original test case by Gerhard Steinmetz.
program main
  character(len=2), dimension(9), parameter :: f = ['("','He','ll','lo',', ','wo','rl','d!','")']
  character(len=2), dimension(9) :: g = ['("','He','ll','lo',', ','wo','rl','d!','")']
  character (len=20) :: line
  write (unit=line,fmt=f)
  if (line /= "Helllo, world!") STOP 1
  line = " "
  write (unit=line,fmt=g)
  if (line /= "Helllo, world!") STOP 2
end program main
