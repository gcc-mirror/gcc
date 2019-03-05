// { dg-do compile }
// { dg-options "-fopenmp -ffat-lto-objects" }

#pragma omp declare simd uniform(b) linear(c, d) linear(uval(e)) linear(ref(f))
int f1 (int a, int b, int c, int &d, int &e, int &f)
{
  a++;
  b++;
  c++;
  d++;
  e++;
  f++;
  return a + b + c + d + e + f;
}

// { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-11 }
// { dg-final { scan-assembler-times "_ZGVbM4vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16vulLUR4__Z2f1iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
                                      

#pragma omp declare simd uniform(b) linear(c, d) linear(uval(e)) linear(ref(f))
int f2 (int a, int b, int c, int &d, int &e, int &f)
{
  asm volatile ("" : : "r" (&a));
  asm volatile ("" : : "r" (&b));
  asm volatile ("" : : "r" (&c));
  asm volatile ("" : : "r" (&d));
  asm volatile ("" : : "r" (&e));
  asm volatile ("" : : "r" (&f));
  a++;
  b++;
  c++;
  d++;
  e++;
  f++;
  return a + b + c + d + e + f;
}

// { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-17 }
// { dg-final { scan-assembler-times "_ZGVbM4vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16vulLUR4__Z2f2iiiRiS_S_:" 1 { target { i?86-*-* x86_64-*-* } } } }

#pragma omp declare simd uniform(b) linear(c, d) linear(uval(e)) linear(ref(f))
int f3 (const int a, const int b, const int c, const int &d, const int &e, const int &f)
{
  return a + b + c + d + e + f;
}

// { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-5 }
// { dg-final { scan-assembler-times "_ZGVbM4vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16vulLUR4__Z2f3iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }

#pragma omp declare simd uniform(b) linear(c, d) linear(uval(e)) linear(ref(f))
int f4 (const int a, const int b, const int c, const int &d, const int &e, const int &f)
{
  asm volatile ("" : : "r" (&a));
  asm volatile ("" : : "r" (&b));
  asm volatile ("" : : "r" (&c));
  asm volatile ("" : : "r" (&d));
  asm volatile ("" : : "r" (&e));
  asm volatile ("" : : "r" (&f));
  return a + b + c + d + e + f;
}

// { dg-warning "GCC does not currently support mixed size types for 'simd' functions" "" { target aarch64*-*-* } .-11 }
// { dg-final { scan-assembler-times "_ZGVbM4vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVbN4vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcM4vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVcN4vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdM8vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVdN8vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeM16vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
// { dg-final { scan-assembler-times "_ZGVeN16vulLUR4__Z2f4iiiRKiS0_S0_:" 1 { target { i?86-*-* x86_64-*-* } } } }
