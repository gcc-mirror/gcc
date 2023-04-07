/* { dg-do compile { target x86_64-*-* } } */

#define STACK_SIZE 4096
struct cpu_info {};
struct cpu_info *get_cpu_info(void)
{
  register unsigned long sp asm("rsp");
  return (struct cpu_info *)((sp | (STACK_SIZE - 1)) + 1) - 1; /* { dg-bogus "use of uninitialized value 'sp'" } */
}
