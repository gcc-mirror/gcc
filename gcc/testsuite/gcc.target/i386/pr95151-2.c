/* { dg-do compile } */
/* { dg-options "-O2 -minline-all-stringops" } */

int
func (void *d, void *s, unsigned int l)
{
  return __builtin_memcmp (d, s, l) ? 1 : 2;
}

/* { dg-final { scan-assembler-not "call\[\\t \]*_?memcmp" } } */
