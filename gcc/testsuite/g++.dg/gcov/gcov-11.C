/* Check that unexecuted exception processing regions are shown
   distinct from  unexecuted normal regions.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

void Baz (int i)
{
  if (i)
    throw 1;
}

void Boz () throw ()
{
}

int main ()
{
  try
    {
      Baz (0);  /* count (1) */
      Baz (0);  /* count (1) */
    }
  catch (...)
    {
      Boz ();  /* count (=====) */
    }

  try
    {
      Baz (1);  /* count (1) */
      Baz (0);  /* count (#####) */
    }
  catch (...)
    {
      Boz ();  /* count (1) */
    }

  return 0;  /* count (1) */
}

/* { dg-final { run-gcov gcov-11.C } } */
