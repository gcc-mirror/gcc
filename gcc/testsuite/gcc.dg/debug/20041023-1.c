/* { dg-do compile } */

static void
foo (unsigned char x)
{
  unsigned char a[5 + x];
}

void
bar (void)
{
  foo (80);
}
