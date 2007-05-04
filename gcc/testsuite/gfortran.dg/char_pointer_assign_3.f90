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
  if(len(textp)  /= 7) call abort()
  if(len(textp2) /= 5) call abort()
  textp  = 'aaaaaaa'
  textp2 = 'bbbbbbb'
  if(textp  /= 'bbbbbaa') call abort()
  if(textp2 /= 'bbbbb') call abort()
end program test
