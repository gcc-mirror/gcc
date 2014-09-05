/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-inline" } */

#include <string.h>
#include <stdlib.h>

#define LEN (100)
short a[LEN];
void
foo (void)
{
    memset (a, -1, 7);
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
  check ((signed char *)a, 7, sizeof (a), -1);

  return 0;
}

/* { dg-final { scan-assembler-not "bl?\[ \t\]*memset" { target { ! arm_thumb1_ok } } } } */
/* { dg-final { scan-assembler-not "strh" { target { arm_unaligned } } } } */
/* { dg-final { scan-assembler-not "strb" { target { arm_unaligned } } } } */
