! { dg-do run }
program ishft_2
  if ( ishftc(3, 2, 3) /= 5 ) STOP 1
  if ( ishftc(256+3, 2, 3) /= 256+5 ) STOP 2
  if ( ishftc(1_4, 31)+1 /= -huge(1_4) ) STOP 3
end program
