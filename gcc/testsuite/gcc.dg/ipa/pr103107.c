/* { dg-do compile } */
/* { dg-options "-Og -g -fipa-sra -fno-tree-dce" } */

typedef int __attribute__((__vector_size__ (8))) V;
V v;

static void
bar (int i)
{
  V l = v + i;
}

void
foo (void)
{
  bar (0);
}
