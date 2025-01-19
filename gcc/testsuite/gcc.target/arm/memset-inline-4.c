/* { dg-do compile } */
/* { dg-skip-if "Don't inline memset using neon instructions" { ! arm_tune_string_ops_prefer_neon } } */
/* { dg-options "-save-temps -O2 -fno-inline" } */
/* { dg-add-options "arm_neon" } */

#include <string.h>
#include <stdlib.h>

#define LEN (100)
int a[LEN];
int b[LEN];
int c[LEN];
void
foo1 (void)
{
    memset (a, -1, 8);
    return;
}

void
foo2 (void)
{
  memset (b, 1, 12);
  return;
}

void
foo3 (void)
{
  memset (c, 1, 13);
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
  int i;

  foo1 ();
  check ((signed char *)a, 8, sizeof (a), -1);

  foo2 ();
  check ((signed char *)b, 12, sizeof (b), 1);

  foo3 ();
  check ((signed char *)c, 13, sizeof (c), 1);

  return 0;
}

/* { dg-final { scan-assembler-not "bl?\[ \t\]+memset" { target { ! arm_thumb1_ok } } } } */
/* { dg-final { scan-assembler-times "vst1\.8" 1 { target { arm_little_endian && arm_neon } } } } */
/* { dg-final { scan-assembler "vstr" { target { arm_little_endian && arm_neon } } } } */
