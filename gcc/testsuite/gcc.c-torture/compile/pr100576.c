/* { dg-require-effective-target non_strict_prototype } */

/* PR middle-end/100576 */

const char v[] = {0x12};

void
foo (const char *p)
{
  int b = sizeof v;
  int n = memcmp (p, v, b);
  if (n)
    __builtin_abort ();
}
