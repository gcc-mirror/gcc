/* Copyright (C) 2002 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Source: Neil Booth, 26 Feb 2002.

   Test that we allow directives in macro arguments.  */

/* { dg-do run } */
/* { dg-options "" } */

#define f(x) x
extern void abort (void);

int main ()
{
  if (f (
#if f(1)			/* True.  */
	0))			/* False. */
#else
    	1))
#endif
     abort ();

     /* Outer f expands to original definition, f in argument expands
	to new definition, so result is: if (1 != 2 - 1).  */
     if (1 != f(2
#undef f
#define f - 1
     f))
     abort ();

     return 0;
}
