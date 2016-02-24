/* { dg-do run } */

int b, c = 1, e, f; 
int a[6][5] = { {0, 0, 0, 0, 0}, {0, 0, 0, 0, 0}, {0, 1, 0, 0, 0} };

void  __attribute__((noinline))
fn1 ()
{
  int d;
  for (b = 0; b < 5; b++)
    for (d = 4; d; d--)
      a[c + 1][b] = a[d + 1][d];
}

int
main ()
{
  fn1 ();

  if (a[2][1] != 0) 
    __builtin_abort (); 

  return 0; 
}
