! { dg-do run }
! { dg-options -Wall }
! PR 30437  Test for Wall
program main
  character (len=40) &
  c
  c = "Hello, &
         world!" ! { dg-warning "Warning: Missing '&' in continued character constant" }
  if (c.ne.&
                                   "Hello, world!")&
                               call abort();end program main

