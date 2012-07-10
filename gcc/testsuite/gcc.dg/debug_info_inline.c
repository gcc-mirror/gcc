/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int g, a;

static int foo ()
{
  int ret = a;

  if (g > -10)
    return 10;

  return ret;
}

int bar()
{
  if (foo())
    return 0;
  else
    return 1;
}

/* { dg-final { scan-assembler "loc 1 19 0\n.*\n.LBB" } } */
