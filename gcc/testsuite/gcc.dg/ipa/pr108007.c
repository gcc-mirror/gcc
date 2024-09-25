/* { dg-do run } */
/* { dg-options "-Os -fno-dce -fno-tree-dce -g" } */
/* { dg-require-effective-target int32plus } */

/* This tests that when IPA-SRA removes a LHS of a call statement which, in the
   original source, is fed into a useless operation which however can trap when
   given nonsensical input, that we remove it even when the user has turned off
   normal DCE.  */

int a, b, d, e, f = 10000000, h;
short c, g;
static int *i() {
  g = f;
 L:
  h = e = ~g;
  g = ~f % g & e;
  if (!g)
    goto L;
  c++;
  while (g < 1)
    ;
  return &a;
}
static void k() {
  int *l, m = 2;
  l = i();
  for (; d < 1; d++)
    m |= *l >= b;
}
int main() {
  k();
  return 0;
}
