/* PR sanitizer/108777 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-sanitize=all -fsanitize=kernel-address" } */
/* { dg-final { scan-assembler-not "__asan_memcpy" } } */
/* { dg-final { scan-assembler-not "__asan_memset" } } */
/* { dg-final { scan-assembler-not "__asan_memmove" } } */

extern void *memcpy (void *, const void *, __SIZE_TYPE__);
extern void *memmove (void *, const void *, __SIZE_TYPE__);
extern void *memset (void *, int, __SIZE_TYPE__);

void
foo (void *p, void *q, int s)
{
  memcpy (p, q, s);
}

void
bar (void *p, void *q, int s)
{
  memmove (p, q, s);
}

void
baz (void *p, int c, int s)
{
  memset (p, c, s);
}
