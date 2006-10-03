! { dg-do run }
! { dg-options -Wampersand }
! PR 19101  Test line continuations and spaces.  Note: the missing ampersand
! before "world" is non standard default behavior.  Use -std=f95, -std=f2003,
! -pedantic, -Wall, or -Wampersand to catch this error
! Submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>.
program main
  character (len=40) &
  c
  c = "Hello, &
         world!" ! { dg-warning "Warning: Missing '&' in continued character constant" }
  if (c.ne.&
                                   "Hello, world!")&
                               call abort();end program main

