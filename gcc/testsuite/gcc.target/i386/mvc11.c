/* { dg-do run } */
/* { dg-require-ifunc "" } */
/* { dg-options "-std=gnu99" } */

__attribute__((noipa)) int 
baz (int (*fn) (void))
{
  asm volatile ("" : "+g" (fn) : : "memory");
  return fn ();
}

__attribute__((target_clones("arch=sandybridge", "default"))) static int
bar (void)
{
  return 1;
}

__attribute__((target_clones("arch=sandybridge", "default"))) int
foo (void)
{
  baz (bar) - 1;
}

int
main ()
{
  foo ();
}
