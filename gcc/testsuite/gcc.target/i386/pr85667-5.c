/* { dg-do run { target ia32 } } */
/* { dg-options "-O2" } */

void abort (void);

typedef struct
{
  float x;
} Float;

Float __attribute__((ms_abi)) fn1 ()
{
  Float v;
  v.x = 3.145;
  return v;
}

Float fn2 ()
{
  Float v;
  v.x = 3.145;
  return v;
}

int main ()
{
  Float a, b;
  a = fn1 ();
  b = fn2 ();
  if (a.x == 3.145f && b.x == 3.145f)
    return 0;
  abort ();
}
