/* { dg-do compile } */
/* { dg-options "-O2" } */

int result __attribute__ ((zero_call_used_regs("all"))); /* { dg-error "attribute applies only to functions" } */
int
__attribute__ ((zero_call_used_regs("gpr-arg-all")))
foo1 (int x) /* { dg-error "unrecognized 'zero_call_used_regs' attribute argument" } */
{
  return (x + 1);
}
int
__attribute__ ((zero_call_used_regs(1)))
foo2 (int x) /* { dg-error "argument not a string" } */
{
  return (x + 2);
}
