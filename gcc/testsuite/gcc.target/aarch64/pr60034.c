/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O" } */

static unsigned long global_max_fast;

void __libc_mallopt (int param_number, int value)
{
 __asm__ __volatile__ ("# %[_SDT_A21]" :: [_SDT_A21] "nor" ((global_max_fast)));
 global_max_fast = 1;
}
