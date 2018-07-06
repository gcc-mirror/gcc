! { dg-do run }
! { dg-options -Wall }
! PR 30437  Test for Wall
program main
  character (len=40) &
  c
  c = "Hello, &
         world!" ! { dg-warning "Missing '&' in continued character constant" }
  if (c.ne.&
                                   "Hello, world!")&
                               STOP 1;end program main

