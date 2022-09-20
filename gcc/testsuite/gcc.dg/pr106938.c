/* { dg-do compile } */
/* { dg-options "-O1 -fno-ipa-pure-const -fno-tree-ccp -Wuninitialized" } */

int n;

void
undefined (void);

__attribute__ ((returns_twice)) int
zero (void)
{
  return 0;
}

void
bar (int)
{
  int i;

  for (i = 0; i < -1; ++i)
    n = 0;
}

__attribute__ ((simd)) void
foo (void)
{
  int uninitialized;

  undefined ();

  while (uninitialized < 1) /* { dg-warning "uninitialized" } */
    {
      bar (zero ());
      ++uninitialized;
    }
}
