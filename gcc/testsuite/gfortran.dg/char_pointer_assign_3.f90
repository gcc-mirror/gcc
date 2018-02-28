! { dg-do run }
! PR fortran/31803
! Assigning a substring to a pointer

program test
  implicit none
  character (len = 7), target :: textt
  character (len = 7), pointer :: textp
  character (len = 5), pointer :: textp2
  textp => textt
  textp2 => textt(1:5)
  if(len(textp)  /= 7) STOP 1
  if(len(textp2) /= 5) STOP 2
  textp  = 'aaaaaaa'
  textp2 = 'bbbbbbb'
  if(textp  /= 'bbbbbaa') STOP 3
  if(textp2 /= 'bbbbb') STOP 4
end program test
