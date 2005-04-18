! PR libfortran/20950
! Original bug-report by Walt Brainerd, The Fortran Company
! { dg-do run }
      character*20 c
      inquire (33, sequential = c)
      if (c .ne. "UNKNOWN") call abort
      end
