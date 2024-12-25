/* Test float on alpha. */

/* { dg-do run } */
/* { dg-options "-mieee" } */

extern void abort(void);
extern int printf(const char *, ...);

typedef int int32_t __attribute__ ((__mode__ (  __SI__ ))) ;
typedef union
{
  float value;
  int32_t word;
} ieee_float_shape_type;

int isinff(float x)
{
  int32_t ix,t;
  ieee_float_shape_type gf_u;
  gf_u.value = x;
  ix = gf_u.word;
  printf ("%x\n", ix);
  t = ix & 0x7fffffff;
  t ^= 0x7f800000;
  t |= -t;
  return ~(t >> 31) & (1 - ((ix & 0x80000000) >> 30));
}

int main ()
{
  float x = 1.0 / 0.0;
  int i = isinff (x);

  if (i == 0)
    abort ();

  printf ("%d\n", i);
  return 0;
}
