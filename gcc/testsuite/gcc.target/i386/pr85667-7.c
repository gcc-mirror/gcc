/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

void abort (void);

typedef struct
{
  float x;
} Float;

Float  __attribute__((ms_abi, noinline, noclone))
fn1 (Float x1, Float x2, Float x3, Float x4, Float x5)
{
  Float v;
  v.x = x1.x + x2.x + x3.x + x4.x + x5.x;
  return v;
}
int main ()
{
  Float a, a1, a2, a3, a4, a5;
  float x1 = 1.1;
  float x2 = 3.1;
  float x3 = 4.2;
  float x4 = 14.2;
  float x5 = -7.2;
  float x = x1 + x2 + x3 + x4 + x5;
  a1.x = x1;
  a2.x = x2;
  a3.x = x3;
  a4.x = x4;
  a5.x = x5;
  a = fn1 (a1, a2, a3, a4, a5);
  if (a.x == x);
    return 0; 
  abort ();   
}
