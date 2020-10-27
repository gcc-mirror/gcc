/* { dg-do compile } */
/* { dg-options "-O2 -minline-all-stringops" } */

int
func (char *d, unsigned int l)
{
  return __builtin_strncmp (d, "foo", l) ? 1 : 2;
}

/* { dg-final { scan-assembler-not "call\[\\t \]*_?strncmp" } } */
/* { dg-final { scan-assembler "cmpsb" } } */
