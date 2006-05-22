/* { dg-options "-maltivec" } */

extern void abort ();

typedef union
{
  int i[4];
  __attribute__((altivec(vector__))) int v;
} vec_int4;

int main (void)
{
  vec_int4 i1;

  i1.v = (__attribute__((altivec(vector__))) int){31, 31, 31, 31};

  if (i1.i[0] != 31)
    abort ();

  return 0;
}

