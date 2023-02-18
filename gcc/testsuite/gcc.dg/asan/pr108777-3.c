/* PR sanitizer/108777 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-sanitize=all -fsanitize=kernel-address --param asan-kernel-mem-intrinsic-prefix=1" } */
/* { dg-final { scan-assembler-not "__asan_memcpy" } } */
/* { dg-final { scan-assembler-not "__asan_memset" } } */
/* { dg-final { scan-assembler-not "__asan_memmove" } } */

extern void *memcpy (void *, const void *, __SIZE_TYPE__);
extern void *memmove (void *, const void *, __SIZE_TYPE__);
extern void *memset (void *, int, __SIZE_TYPE__);

__attribute__((no_sanitize("kernel-address"))) void
foo (void *p, void *q, int s)
{
  memcpy (p, q, s);
}

__attribute__((no_sanitize("kernel-address"))) void
bar (void *p, void *q, int s)
{
  memmove (p, q, s);
}

__attribute__((no_sanitize("kernel-address"))) void
baz (void *p, int c, int s)
{
  memset (p, c, s);
}
