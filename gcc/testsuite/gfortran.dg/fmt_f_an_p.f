! { dg-do run }
! PR38285 wrong i/o output: interaction between f and p for output
! Special case of kPFw.d when d = 0
      program f_and_p
      character(28) string
      write(string,1) 3742. , 0.3742
    1 format ( f14.0, 4pf14.0 )
      if (string.ne."         3742.         3742.") call abort
      end program f_and_p

