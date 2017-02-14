typedef unsigned char V8 __attribute__ ((vector_size (32)));
typedef unsigned int V32 __attribute__ ((vector_size (32)));
typedef unsigned long long V64 __attribute__ ((vector_size (32)));

static V32 __attribute__ ((noinline, noclone))
foo (V64 x)
{
  V64 y = (V64)(V8){((V8)(V64){65535, x[0]})[1]};
  return (V32){y[0], 255};
}

int main ()
{
  V32 x = foo ((V64){});
//  __builtin_printf ("%08x %08x %08x %08x %08x %08x %08x %08x\n", x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]);
  if (x[1] != 255)
    __builtin_abort();
  return 0;
}

