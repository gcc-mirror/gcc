/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */
/* { dg-final { scan-assembler-not "memcpy" } } */

void
test (void *dst, void *src)
{
  __builtin_memcpy (dst, src, sizeof (char *) / 2);
}
