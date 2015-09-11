/* { dg-do compile } */
/* { dg-options "-O3 -fwhole-program -march=x86-64" } */

int *a;
unsigned int b;

void fn2 ()
{
  int t[9];
  for (; b; b++)
    *a ^= (~t[b] != t[b]);
}

int fn1 ()
{
  fn2 (); 
  return 0; 
}

int main ()
{
  fn1 (); 
  return 0;
}
