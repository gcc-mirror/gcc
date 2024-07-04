/* { dg-do run } */
/* { dg-additional-options "-fmerge-all-constants" } */

char *p;

char * __attribute__((noipa))
foo () { return p+1; }

volatile int z;

int main()
{
  /* ESCAPED = CONST_POOL */
  p = "Hello";
  /* PT = ESCAPED */
  char *x = foo ();
  char *y;
  /* y PT = CONST_POOL */
  if (z)
    y = "Baz";
  else
    y = "Hello" + 1;
  if (y != x)
    __builtin_abort ();
  return 0;
}
