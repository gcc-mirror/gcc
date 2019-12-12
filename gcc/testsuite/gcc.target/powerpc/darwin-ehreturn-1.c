/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mdejagnu-cpu=G3 -funwind-tables" } */
/* { dg-final { scan-assembler "bl save_world" } } */
/* { dg-final { scan-assembler ".byte\t0x6b" } } */

/* Verify that on Darwin, even with -mcpu=G3, __builtin_eh_return
   saves Altivec registers using save_world, and reports their
   location in its EH information.  */

long offset;
void *handler;

extern void setup_offset(void);

void foo(void)
{
  __builtin_unwind_init ();
  setup_offset();
  __builtin_eh_return (offset, handler);
}
