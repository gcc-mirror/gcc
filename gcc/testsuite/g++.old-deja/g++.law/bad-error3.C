// { dg-do assemble  }
// GROUPS passed bad-errors
// bad-error file
// Date: Sun, 31 Jul 1994 11:37:43 +1000 (EST)
// From: Rohan LENARD <rjl@iassf.easams.com.au>
// Subject: g++-2.6.0 gives wrong warning for placement syntax new
// Message-Id: <0iCk1b0000000z0VY0@iassf.easams.com.au>


#include <stddef.h>

void * operator new(size_t, int *);
void * operator new(size_t, void *);

int *x = 0;
int foo(){
new (x) int *;
new (&x) int *;
new (x) int *;  // This is identical to line 8 !!!
return 1;
}
