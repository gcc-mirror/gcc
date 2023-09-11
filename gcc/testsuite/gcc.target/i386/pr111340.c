/* PR target/111340 */
/* { dg-do compile { target { fpic && int128 } } } */
/* { dg-options "-O2 -fpic" } */

void
bar (void)
{
  __asm ("# %0" : : "g" ((((unsigned __int128) 0x123456789abcdef0ULL) << 64) | 0x0fedcba987654321ULL));
}
