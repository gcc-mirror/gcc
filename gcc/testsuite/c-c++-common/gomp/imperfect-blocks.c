/* { dg-do compile } */

/* Check that compound statements in intervening code are correctly
   handled.  */

extern void do_something (void);

void imperfect1 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    {
      {}
      for (int j = 0; j < y; j++)
	do_something ();
    }
}

void perfect1 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      {}
      for (int j = 0; j < y; j++)
	do_something ();
    }
}

void imperfect2 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    {
      for (int j = 0; j < y; j++)
	do_something ();
      {}
    }
}

void perfect2 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      for (int j = 0; j < y; j++)
	do_something ();
      {}
    }
}

void imperfect3 (int x, int y)
{
#pragma omp for collapse (2)
  for (int i = 0; i < x; i++)
    {
      { do_something (); }
      for (int j = 0; j < y; j++)
	do_something ();
      { do_something (); }
    }
}

void perfect3 (int x, int y)
{
#pragma omp for ordered (2)
  for (int i = 0; i < x; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      { do_something (); }
      for (int j = 0; j < y; j++)
	do_something ();
      { do_something (); }
    }
}

