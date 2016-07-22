/* { dg-do run } */
/* { dg-require-effective-target cilkplus_runtime } */
/* { dg-options "-fcilkplus" } */
/* { dg-additional-options "-std=gnu99" { target c } } */


int grain_value = 2;
int main (void)
{
  int Array1[200], Array1_Serial[200];

  for (int ii = 0; ii < 200; ii++)
    {
      Array1_Serial[ii] = 2;
      Array1[ii] = 1;
    }

#pragma cilk grainsize = 2
  _Cilk_for (int ii = 0; ii < 200; ii++)
    Array1[ii] = 2;

  for (int ii = 0; ii < 200; ii++)
    if (Array1[ii] != Array1_Serial[ii])
      return (ii+1);

#pragma cilk grainsize = grain_value
  _Cilk_for (int ii = 0; ii < 200; ii++)
    Array1[ii] = 2;

  for (int ii = 0; ii < 200; ii++)
    if (Array1[ii] != Array1_Serial[ii])
      return (ii+1);

  return 0;
}
