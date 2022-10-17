/* PR preprocessor/102432 */

#define loop(x)

void
foo (void)
{
  int i;
#pragma omp parallel
#pragma omp loop
  for (i = 0; i < 64; i++)
    ;
}

void
bar (void)
{
  int i;
  _Pragma ("omp parallel")
  _Pragma ("omp loop")
  for (i = 0; i < 64; i++)
    ;
}
