/* PR tree-optimization/114278 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -fno-tree-dce -fno-tree-dse -fno-tree-ccp" } */
/* { dg-additional-options "-mavx2" { target i?86-*-* x86_64-*-* } } */

void
foo (void *p)
{
  _BitInt(64) b = *(_BitInt(64) *) __builtin_memmove (&b, p, sizeof (_BitInt(64)));
}

#if __BITINT_MAXWIDTH__ >= 128
void
bar (void *p)
{
  _BitInt(128) b = *(_BitInt(128) *) __builtin_memmove (&b, p, sizeof (_BitInt(128)));
}
#endif

#if __BITINT_MAXWIDTH__ >= 256
void
baz (void *p)
{
  _BitInt(256) b = *(_BitInt(256) *) __builtin_memmove (&b, p, sizeof (_BitInt(256)));
}
#endif
