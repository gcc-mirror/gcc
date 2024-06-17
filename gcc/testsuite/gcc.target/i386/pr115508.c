/* { dg-do compile } */
/* { dg-options "-O2 -march=znver1" } */

typedef long long v4di __attribute__((vector_size(4 * sizeof (long long))));

v4di vec_var;
extern long long array1[];
long long g(void)
{
  int total_error_4 = 0;
  total_error_4 += array1 [0] + array1 [1] + array1 [2] + array1 [3];
  v4di t = vec_var;
  long long iorvar = t [1] | t [0] | t [2] | t [3];
  return iorvar + total_error_4;
}
