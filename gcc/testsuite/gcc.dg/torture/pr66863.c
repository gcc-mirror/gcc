/* { dg-do run } */

int a, b;

int
fn1 (int p1)
{
  if (p1 < -2147483647) 
    return 0;
  else 
    return 1;
}

int
fn2 (int p1, short p2)
{
  return p2 ? p1 % p2 : 0; 
}

int
main ()
{
  b = fn2 (fn1 (a), a);
  return 0;
}
