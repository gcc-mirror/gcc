/* { dg-do run } */
/* { dg-additional-options "-mno-speculate-indirect-jumps" } */

/* Test for deliberate misprediction of indirect calls for ELFv2.  */

int (*f)();

int __attribute__((noinline)) bar ()
{
  return (*f) ();
}

int g ()
{
  return 26;
}

int main ()
{
  f = &g;
  if (bar () != 26)
    __builtin_abort ();

  return 0;
}
