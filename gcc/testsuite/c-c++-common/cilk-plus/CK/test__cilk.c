/* { dg-do compile } */
/* { dg-do run { target cilkplus_runtime } } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  if (__cilk == 200)
   return 0; 
  return 1;
}
