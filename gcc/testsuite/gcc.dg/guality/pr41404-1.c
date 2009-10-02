/* PR debug/41404 */
/* { dg-do run } */
/* { dg-options "-g" } */

__attribute__ ((noinline))
int bar1 (int i)
{
  const char *foo = "foo";
  asm volatile ("" : "+r" (i) : : "memory");
  i++;	/* { dg-final { gdb-test 10 "*foo" "'f'" } } */
  asm volatile ("" : "+r" (i) : : "memory");
  foo = "bar";
  asm volatile ("" : "+r" (i) : : "memory");
  i++;	/* { dg-final { gdb-test 14 "*foo" "'b'" } } */
  asm volatile ("" : "+r" (i) : : "memory");
  return i;
}

__attribute__ ((noinline))
int bar2 (int i)
{
  const char *foo = "foo";
  asm volatile ("" : "+r" (i) : : "memory");
  i++;	/* { dg-final { gdb-test 24 "*foo" "'f'" } } */
  asm volatile ("" : "+r" (i) : : "memory");
  return i;
}

__attribute__ ((noinline))
const char *baz (int i)
{
  return i ? "foo" : "bar";
}

int
main (void)
{
  bar1 (6);
  bar2 (6);
  return 0;
}
