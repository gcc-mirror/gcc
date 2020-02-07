/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

void abort (void);

typedef struct
{
  double x;
} Double;

Double  __attribute__((ms_abi, noinline, noclone))
fn1 (Double x1, Double x2, Double x3, Double x4, Double x5)
{
  Double v;
  v.x = x1.x + x2.x + x3.x + x4.x + x5.x;
  return v;
}
int main ()
{
  Double a, a1, a2, a3, a4, a5;
  double x1 = 1.1;
  double x2 = 3.1;
  double x3 = 4.2;
  double x4 = 14.2;
  double x5 = -7.2;
  double x = x1 + x2 + x3 + x4 + x5;
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
