/* Bug 9862 -- Spurious warnings with -finline-functions.
   Copyright (C) 2003 Free Software Foundation Inc.  */

/* { dg-do compile } */
/* { dg-options "-fpermissive -O -finline-functions -Wreturn-type" } */

extern int i;
extern int foo (void);
extern int bar (void);

int foo (void)
{
  if( i ) return; /* { dg-warning "'return' with no value, in function returning non-void" } */
  else    return 1;
}

int bar (void)
{
  if( i ) return 0;
  else    return 1;
}
