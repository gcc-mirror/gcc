/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */

typedef long (*fptr_t) (void);
long
func (fptr_t *p)
{
  if (p)
    return (*p) ();
  return 0;
}
/* { dg-final { scan-assembler-not {mr %?r?12,} } } */
