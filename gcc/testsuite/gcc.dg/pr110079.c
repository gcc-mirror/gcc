/* PR rtl-optimization/110079 */
/* { dg-do compile { target { nvptx-*-* || lra } } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-freorder-blocks-and-partition" { target freorder } } */

int a;
__attribute__((cold)) int bar (char *);
__attribute__((hot)) int baz (char *);

void
foo (void)
{
l1:
  while (a)
    ;
  bar ("");
  asm goto ("" : : : : l2);
  asm ("");
l2:
  goto l1;
}

void
qux (void)
{
  asm goto ("" : : : : l1);
  bar ("");
  goto l1;
l1:
  baz ("");
}

void
corge (void)
{
  asm goto ("" : : : : l1);
  baz ("");
l2:
  return;
l1:
  bar ("");
  goto l2;
}
