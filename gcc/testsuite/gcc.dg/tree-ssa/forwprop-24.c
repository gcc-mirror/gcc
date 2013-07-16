/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

void bar (void);
unsigned short
foo (unsigned char x, unsigned short y)
{
  unsigned char t = (unsigned char)((x & 1) ^ ((unsigned char)y & 1));
  if (t == 1)
    bar ();
  return y;
}

/* We should have combined this to require only one bitwise and
   as in (x ^ (char) y) & 1.  */

/* { dg-final { scan-tree-dump-times " & " 1 "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */
