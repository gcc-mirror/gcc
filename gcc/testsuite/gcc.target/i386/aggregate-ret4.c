/* Check that, with dont_keep_aggregate_return_pointer attribute,  callee
   pops the stack for the implicit pointer arg when returning a large
   structure in memory.  */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */

struct foo {
  int a;
  int b;
  int c;
  int d;
};

__attribute__ ((sysv_abi))
struct foo
bar (void)
{
  struct foo retval;
  retval.a = 1;
  retval.b = 2;
  retval.c = 3;
  retval.d = 4;
  return retval;
}

/* { dg-final { scan-assembler "ret\[ \t\]\\\$4" } } */


