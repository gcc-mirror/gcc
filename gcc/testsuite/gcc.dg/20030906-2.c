/* Bug 9862 -- Spurious warnings with -finline-functions.
   Copyright (C) 2003 Free Software Foundation Inc.  */

/* { dg-do compile } */
/* { dg-options "-O -finline-functions -Wextra" } */

extern int i;
extern int foo (void);
extern int bar (void);

int foo (void)
{
  if( i ) return;
  else    return 1;
}		/* { dg-warning "may return with or without a value" } */

int bar (void)
{
  if( i ) return 0;
  else    return 1;
}
