/* { dg-do compile } */
/* { dg-options "-fcilkplus -Wall" } */

__attribute__((vector (linear (x:y)))) /* { dg-message "parameter" "" { target c++ } } */
int func2 (int x, int y) 
{ /* { dg-message "using parameters for" "" { target c } } */
  return (x+y);
}

int main (void)
{
  return (func2 (5,6));
}
