/* { dg-options "-std=gnu23 -Wno-overflow -Wno-div-by-zero" } */

enum e : bool { X };

int
main (void)
{
  if (!(enum e)(__INT_MAX__ + 1) / !(enum e)(__INT_MAX__ + 1))
    ;
  return 1;
}
