/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mabi_ilp32 } */
/* { dg-options "-mcmodel=tiny -mabi=ilp32 -Wno-deprecated -fPIC" } */
/* { dg-require-effective-target fpic } */

extern int bar (void *);
extern long long a;

int
foo (void)
{
  a = 1;
  return bar ((void *)bar);
}

