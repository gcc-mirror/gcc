/* { dg-do run { target lp64 } } */
/* { dg-options "-O2" } */

void abort (void);

typedef struct
{
  double x;
} Double;

Double __attribute__((ms_abi)) fn1 ()
{
  Double v;
  v.x = 3.145;
  return v;
}

Double fn2 ()
{
  Double v;
  v.x = 3.145;
  return v;
}

int main ()
{
  Double a, b;
  a = fn1 ();
  b = fn2 ();
  if (a.x == 3.145 && b.x == 3.145)
    return 0; 
  abort ();   
}
