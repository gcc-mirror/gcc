/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus -Wall" } */

__attribute__((vector (linear (x:y))))
int func2 (int x, int y) 
{ /* { dg-message "using parameters for" } */
  return (x+y);
}

int main (void)
{
  return (func2 (5,6));
}
