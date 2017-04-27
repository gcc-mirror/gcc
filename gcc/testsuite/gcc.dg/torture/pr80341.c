/* { dg-do run } */
/* { dg-additional-options "-Wno-overflow" } */
/* { dg-require-effective-target int32plus } */

const signed char c = -84;
signed char s;

void
foo ()
{
  s = (unsigned short) c / -55;
}

int
main ()
{
  foo ();
  if (s != 90)
    __builtin_abort ();
}
