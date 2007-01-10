! { dg-do run }
program ishft_2
  if ( ishftc(3, 2, 3) /= 5 ) call abort()
  if ( ishftc(256+3, 2, 3) /= 256+5 ) call abort()
  if ( ishftc(1_4, 31)+1 /= -huge(1_4) ) call abort()
end program
