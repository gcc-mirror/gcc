/* { dg-do compile } */
/* { dg-additional-options "-fwrapv" } */

int f(long l, short *sp) {
  unsigned short us;
  for (; l; l -= 4, sp += 4)
    us += sp[1] + sp[3];
  return us;
}
