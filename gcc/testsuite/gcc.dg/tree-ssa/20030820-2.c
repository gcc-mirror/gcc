/* { dg-do compile } */
/* { dg-options "-O1" } */

/* A test for variables getting out of their scope in copy propagation.  */

void foo(void)
{
  int k;

  goto forward;
back:
  bla (k);
  return;

forward:
    {
      int i = bar ();

      k = i;

      goto back;
    }
}

