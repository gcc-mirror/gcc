/* { dg-do compile } */

/* Check that non-statement pragmas are accepted in a canonical loop nest
   even when perfect nesting is required.  */

extern void do_something (void);

void imperfect1 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    {
#pragma GCC diagnostic push
      for (int j = 0; j < y; j++)
	do_something ();
#pragma GCC diagnostic pop
    }
}

void perfect1 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)
    {
#pragma GCC diagnostic push
      for (int j = 0; j < y; j++)
	do_something ();
#pragma GCC diagnostic pop
    }
}


/* "GCC unroll" is a statement pragma that consumes the following loop as
   a substatement.  Thus, the inner loop should be treated as intervening
   code rather than part of the loop nest.  */

void imperfect2 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    {
#pragma GCC unroll 4
      for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	do_something ();
    }
}

void perfect2 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    /* { dg-error "inner loops must be perfectly nested" "" { target *-*-*} .-1 } */
    {
#pragma GCC unroll 4
      for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	do_something ();
    }
}


/* Check that statement pragmas are accepted in the innermost loop body.  */

void imperfect3 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    for (int j = 0; j < y; j++)
      {
#pragma GCC unroll 4
	for (int k = 0; k < 4; k++)
	  do_something ();
      }
}

void perfect3 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)
    for (int j = 0; j < y; j++)
      {
#pragma GCC unroll 4
	for (int k = 0; k < 4; k++)
	  do_something ();
      }
}
