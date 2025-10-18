/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve2_hw }  */
/* { dg-options "-O3 -march=armv8-a+sve2 -mautovec-preference=sve-only -fdump-tree-vect-details" }*/

inline char char_abs(char i) {
  return (i < 0 ? -i : i);
}

__attribute__((noipa))
int foo_int(unsigned char *x, unsigned char * restrict y) {
  int sum = 0;
  for (int i = 0; i < 100; i++)
     sum += char_abs(x[i] - y[i]);
  return sum;
}

__attribute__((noipa))
int foo2_int(unsigned short *x, unsigned short * restrict y,
	     unsigned short * restrict z) {
  int sum = 0;
  for (int i = 0; i < 100; i++)
    {
      z[i] = x[i] + y[i];
      sum += z[i];
    }
  return sum;
}

__attribute__((noipa))
int foo_int2(unsigned char *x, unsigned char * restrict y) {
  int sum = 0;
#pragma GCC novector
  for (int i = 0; i < 100; i++)
     sum += char_abs(x[i] - y[i]);
  return sum;
}

__attribute__((noipa))
int foo2_int2(unsigned short *x, unsigned short * restrict y,
	      unsigned short * restrict z) {
  int sum = 0;
#pragma GCC novector
  for (int i = 0; i < 100; i++)
    {
      z[i] = x[i] + y[i];
      sum += z[i];
    }
  return sum;
}

int main ()
{
  unsigned short a[100];
  unsigned short b[100];
  unsigned short r1[100];
  unsigned short r2[100];
  unsigned char c[100];
  unsigned char d[100];
#pragma GCC novector
  for (int i = 0; i < 100; i++)
    {
      a[i] = c[i] = i;
      b[i] = d[i] = 100 - i;
    }

  if (foo_int (c, d) != foo_int2 (c, d))
    __builtin_abort();


  if (foo2_int (a, b, r1) != foo2_int2 (a, b, r2))
    __builtin_abort();

#pragma GCC novector
  for (int i = 0; i < 100; i++)
    if (r1[i] != r2[i])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 2 "vect" } } */