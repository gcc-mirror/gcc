/* float reductions.  */

#define vl 32

int
main(void)
{
  const int n = 1000;
  int i;
  float result, array[n];
  int lresult;

  /* '+' reductions.  */
#pragma acc parallel vector_length (vl)
#pragma acc loop reduction (+:result)
  for (i = 0; i < n; i++)
    result += array[i];

  /* '*' reductions.  */
#pragma acc parallel vector_length (vl)
#pragma acc loop reduction (*:result)
  for (i = 0; i < n; i++)
    result *= array[i];

//   /* 'max' reductions.  */
// #pragma acc parallel vector_length (vl)
// #pragma acc loop reduction (+:result)
//   for (i = 0; i < n; i++)
//       result = result > array[i] ? result : array[i];
// 
//   /* 'min' reductions.  */
// #pragma acc parallel vector_length (vl)
// #pragma acc loop reduction (+:result)
//   for (i = 0; i < n; i++)
//       result = result < array[i] ? result : array[i];

  /* '&&' reductions.  */
#pragma acc parallel vector_length (vl)
#pragma acc loop reduction (&&:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult && (result > array[i]);

  /* '||' reductions.  */
#pragma acc parallel vector_length (vl)
#pragma acc loop reduction (||:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult || (result > array[i]);

  return 0;
}
