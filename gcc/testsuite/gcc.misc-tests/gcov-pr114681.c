/* { dg-do compile } */
/* { dg-options "-O -fnon-call-exceptions -fno-exceptions -fcondition-coverage" } */

float f, g;

static void
bar ()
{
  if (g < f)
    for (;;)
      ;
}

void
foo ()
{
  bar ();
}
