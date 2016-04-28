/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */



/* <feature> loop control variable must have integer, pointer or class type
   </feature>
*/

#define ARRAY_SIZE 10000
int a[ARRAY_SIZE];

int main(void)
{
  int ii = 0;

  for (ii =0; ii < ARRAY_SIZE; ii++)
    a[ii] = 5;
  _Cilk_for(int *aa = a; aa < a + ARRAY_SIZE; aa++)
    *aa = 0;
  for (ii = 0; ii < ARRAY_SIZE; ii++)
    if (a[ii] != 0)
      __builtin_abort ();

  _Cilk_for (int *aa = a; aa < a + ARRAY_SIZE; aa = aa + 2)
    *aa = 4;

  for (ii = 0; ii < ARRAY_SIZE; ii = ii + 2)
    if (a[ii] != 4)
      __builtin_abort ();

  return 0;
}
