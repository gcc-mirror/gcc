/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

/* ??? Irrespective of insn set, generated code for this is a mess.  */
struct foo { unsigned a: 3, b: 8, c: 21; };

struct foo
f (struct foo i)
{
  i.b = 42;
  return i;
}

struct foo
g (struct foo i, int j)
{
  i.b = j;
  return i;
}
/* { dg-final { scan-assembler "movbi\[ \t\]" } } */
/* { dg-final { scan-assembler "movb\[ \t\]" } } */
