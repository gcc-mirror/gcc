/* { dg-do compile } */

/* Check that a nested FOR loop with a label on it is treated as
   intervening code, since it doesn't match the grammar for canonical
   loop nest form.  */

extern void do_something (void);

void imperfect1 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    {
    foo:
      for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	do_something ();
    }
}

void perfect1 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    /* { dg-error "inner loops must be perfectly nested" "" { target *-*-*} .-1 } */
    {
    foo:
      for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	do_something ();
    }
}


/* Similar, but put the label on a block wrapping the nested loop instead.  */

void imperfect2 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    {
    foo:
      {
	for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	  do_something ();
      }
    }
}

void perfect2 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "not enough nested loops" } */
    /* { dg-error "inner loops must be perfectly nested" "" { target *-*-*} .-1 } */
    {
    foo:
      {
	for (int j = 0; j < y; j++)  /* { dg-error "loop not permitted in intervening code" } */
	  do_something ();
      }
    }
}

/* Sanity check that labels are allowed in the innermost loop body.  */

void imperfect3 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    for (int j = 0; j < y; j++)
      {
      foo:
	do_something ();
      }
}

void perfect3 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)
    for (int j = 0; j < y; j++)
      {
      foo:
	do_something ();
      }
}

