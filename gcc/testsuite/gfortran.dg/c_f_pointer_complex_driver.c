/* { dg-options "-std=c99 -w" } */
/* From c_by_val.c in gfortran.dg.  */
#define _Complex_I (1.0iF)

#define NUM_ELEMS 10

void test_complex_scalars (float _Complex *float_complex_ptr,
                           double _Complex *double_complex_ptr,
                           long double _Complex *long_double_complex_ptr);
void test_complex_arrays (float _Complex *float_complex_array,
                          double _Complex *double_complex_array,
                          long double _Complex *long_double_complex_array,
                          int num_elems);

int main (int argc, char **argv)
{
  float _Complex c1;
  double _Complex c2;
  long double _Complex c3;
  float _Complex c1_array[NUM_ELEMS];
  double _Complex c2_array[NUM_ELEMS];
  long double _Complex c3_array[NUM_ELEMS];
  int i;

  c1 = 1.0 + 0.0 * _Complex_I;
  c2 = 2.0 + 0.0 * _Complex_I;
  c3 = 3.0 + 0.0 * _Complex_I;

  test_complex_scalars (&c1, &c2, &c3);

  for (i = 0; i < NUM_ELEMS; i++)
    {
      c1_array[i] = 1.0 * (i+1) + 0.0 * _Complex_I;
      c2_array[i] = 1.0 * (i+1) + 0.0 * _Complex_I;
      c3_array[i] = 1.0 * (i+1) + 0.0 * _Complex_I;
    }

  test_complex_arrays (c1_array, c2_array, c3_array, NUM_ELEMS);

  return 0;
}
