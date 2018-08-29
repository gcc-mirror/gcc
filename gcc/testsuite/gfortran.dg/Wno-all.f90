! PR 30437  Test for negative Wall
! { dg-do run }
! { dg-options "-Wall -Wno-all" }
program main
  character (len=40) &
  c
  c = "Hello, &
         world!" ! { dg-bogus "Warning: Missing '&' in continued character constant" }
  if (c.ne.&
                                   "Hello, world!")&
                               STOP 1;end program main

