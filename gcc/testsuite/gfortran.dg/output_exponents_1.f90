! { dg-do run }
! PR 21376
! we used to take the logarithm of zero in this special case
  character*10 c
  write (c,'(e10.4)') 1.0
  if(c /= "0.1000E+01") call abort
  write (c,'(e10.4)') 0.0
  if(c /= "0.0000E+00") call abort
  write (c,'(e10.4)') 1.0d100
  if(c /= "0.1000+101") call abort
  write (c,'(e10.4)') 1.0d-102
  if(c /= "0.1000-101") call abort
end
