/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */

void h() __attribute__((__noreturn__));
struct Extremes {
  int w;
  int h;
};
struct Extremes *array;
int f(int num, int size1)
{
  int sw = 0, sh = 0;
  for (int i = 0; i < size1; ++i)
  {
    if (num - i == 0)
      h();
    sw += array[i].w;
    sh += array[i].h;
  }
  return (sw) +  (sh);
}
