/* { dg-options "-O2 -funroll-loops -fno-stack-protector" } */

typedef int v2 __attribute__((vector_size(8)));

void f(void) {
  v2 v[1024];
  v2 *ptr = v;
  for (int i = 0; i < 512; ++i)
    {
      ptr[0][0] = 0;
      asm volatile ("":::"memory");
      ptr[0][1] = 1;
      ptr += 2;
    }
}

/* { dg-final { scan-assembler-not {\tstp\t} } } */
