/* This is a copy of pr71727.c with scanning reversed.  */
/* { dg-do compile } */
/* { dg-options "-mstrict-align -O3 -mno-strict-align" } */

struct test_struct_s
{
  long a;
  long b;
  long c;
  long d;
  unsigned long e;
};


char _a;
struct test_struct_s xarray[128];

void
_start (void)
{
  struct test_struct_s *new_entry;

  new_entry = &xarray[0];
  new_entry->a = 1;
  new_entry->b = 2;
  new_entry->c = 3;
  new_entry->d = 4;
  new_entry->e = 5;

  return;
}
/* Should have only 1 mov instead of 5 and should not use stp (store pair).  */
/* { dg-final { scan-assembler-times "mov\tx" 1 {target lp64} } } */
/* { dg-final { scan-assembler-not "stp\tx\[0-9\]+, x\[0-9\]+," {target lp64} } } */
