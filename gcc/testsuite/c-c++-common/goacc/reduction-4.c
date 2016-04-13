/* complex reductions.  */

#define n 1000

int
main(void)
{
  int i;
  __complex__ double result, array[n];
  int lresult;

  /* '+' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (+:result)
  for (i = 0; i < n; i++)
    result += array[i];

  /* '*' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (*:result)
  for (i = 0; i < n; i++)
    result *= array[i];

  /* '&&' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (&&:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult && (__real__(result) > __real__(array[i]));

  /* '||' reductions.  */
#pragma acc parallel
#pragma acc loop gang worker vector reduction (||:lresult)
  for (i = 0; i < n; i++)
    lresult = lresult || (__real__(result) > __real__(array[i]));

  return 0;
}
