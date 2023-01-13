/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp" } */

void bar2 (unsigned char, unsigned char);

void
foo1 (char c)
{
  unsigned char t = c;
  t *= 2;
  unsigned char t1 = t << 7;
  bar2 (t, t1);
}
