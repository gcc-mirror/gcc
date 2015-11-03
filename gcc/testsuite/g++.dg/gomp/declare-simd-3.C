// { dg-do compile }

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

#pragma omp declare simd uniform(b) linear(c, d) linear(uval(e)) linear(ref(f))
int f3 (const int a, const int b, const int c, const int &d, const int &e, const int &f)
{
  return a + b + c + d + e + f;
}

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
