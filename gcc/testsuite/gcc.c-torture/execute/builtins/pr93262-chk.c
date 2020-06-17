/* PR tree-optimization/93262 */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern void *memcpy (void *, const void *, size_t);
extern void *memset (void *, int, size_t);

#include "chk.h"

char b[32] = "def";
char a[32] = "abc";
char c[32] = "ghi";
int l1;

__attribute__((noipa, noinline, noclone, optimize ("tree-dse"))) void
foo (char *b)
{
  memcpy (a, b, 48);
  memset (a, ' ', 16);
}

__attribute__((noipa, noinline, noclone, optimize ("tree-dse"))) void
bar (void)
{
  memset (a, ' ', 48);
  memset (a, '0', 16);
}

void
main_test (void)
{
#ifndef __OPTIMIZE__
  /* Object size checking is only intended for -O[s123].  */
  return;
#endif
  __asm ("" : "=r" (l1) : "0" (l1));
  chk_calls = 0;
  chk_fail_allowed = 1;
  /* Runtime checks.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      foo ("0123456789abcdeffedcba9876543210ghijklmnopqrstuv");
      if (!l1)
	abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      bar ();
      if (!l1)
	abort ();
    }
  if (chk_calls != 2)
    abort ();
  return 0;
}
