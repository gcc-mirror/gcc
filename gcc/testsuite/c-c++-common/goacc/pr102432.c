/* PR preprocessor/102432 */

#define loop(x)

void
foo (void)
{
  int i;
#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 64; i++)
    ;
}

void
bar (void)
{
  int i;
  _Pragma ("acc parallel")
  _Pragma ("acc loop")
  for (i = 0; i < 64; i++)
    ;
}
