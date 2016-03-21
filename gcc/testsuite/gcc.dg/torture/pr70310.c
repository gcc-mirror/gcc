/* { dg-do run } */

typedef unsigned char v32u8 __attribute__ ((vector_size (32)));

unsigned __attribute__((noinline, noclone))
foo(unsigned u)
{
  v32u8 v32u8_0 = (v32u8){} > (v32u8){-u};
  return v32u8_0[31] + v32u8_0[0];
}

int
main ()
{
  unsigned x = foo(0);
  __builtin_printf ("%08x\n",x);
  if (x != 0)
    __builtin_abort();
  return 0;
}
