/* { dg-do compile } */
/* { dg-options "-O1 -fharden-compares -fno-inline -fno-ipa-pure-const" } */

__attribute__ ((pure, returns_twice)) int
bar (int);

int
quux (void)
{
  return 0;
}

int
foo (short int x)
{
  x = !x;
  bar (quux ());

  return x;
}
