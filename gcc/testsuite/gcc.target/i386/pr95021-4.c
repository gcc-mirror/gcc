/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mstv -W" } */
/* { dg-final { scan-assembler "(movaps|vmovdqa)\[ \t\]%xmm\[0-9\]+, \\(%rsp\\)" } } */

#include <setjmp.h>

extern jmp_buf buf;

extern __int128 *target_p;
__int128 *c;

extern int count;

extern void foo (__int128, __int128, __int128, __int128);

__attribute__ ((noclone, noinline))
void
bar (void)
{
  if (setjmp (buf))
    {
      __int128 target = *target_p;
      *c = target;
      foo (0xbadbeef1, 0x2badbeef, 0xbad3beef, target);
    }
  else
    foo (0xbadbeef1, 0x2badbeef, 0xbad3beef, 0);
}
