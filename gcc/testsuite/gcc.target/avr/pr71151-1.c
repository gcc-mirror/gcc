/* { dg-do compile } */
/* { dg-options "-Os -ffunction-sections -fdata-sections" } */

/* { dg-final { scan-assembler-not ".section	.progmem.gcc_sw_table.foo.str1.1" } } */
/* { dg-final { scan-assembler ".section	.rodata.foo.str1.1,\"aMS\"" } } */


extern void bar(const char*);
void foo(void)
{
  bar("BBBBBBBBBB");
}
