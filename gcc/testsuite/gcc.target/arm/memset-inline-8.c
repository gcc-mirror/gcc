/* { dg-do run } */
/* { dg-skip-if "Don't inline memset using neon instructions on cortex-a9" { *-*-* } { "-mcpu=cortex-a9" } { "" } } */
/* { dg-skip-if "Don't inline memset using neon instructions on cortex-a9" { *-*-* } { "-mtune=cortex-a9" } { "" } } */
/* { dg-options "-save-temps -O2 -fno-inline"  } */
/* { dg-add-options "arm_neon" } */

#include <string.h>
#include <stdlib.h>

#define LEN (100)
short a[LEN];
void
foo (void)
{
    memset (a, -1, 14);
    return;
}

void
check (signed char *arr, int idx, int len, int v)
{
  int i;
  for (i = 0; i < idx; i++)
    if (arr[i] != v)
      abort ();

  for (i = idx; i < len; i++)
    if (arr[i] != 0)
      abort ();
}

int
main(void)
{
  foo ();
  check ((signed char *)a, 14, sizeof (a), -1);

  return 0;
}

/* { dg-final { scan-assembler-not "bl?\[ \t\]*memset" { target { arm_thumb2_ok } } } } */
/* { dg-final { scan-assembler "vst1" { target { arm_little_endian && arm_neon } } } } */
/* { dg-final { scan-assembler-not "vstr" { target { arm_little_endian && arm_neon } } } } */
/* { dg-final { cleanup-saved-temps } } */
