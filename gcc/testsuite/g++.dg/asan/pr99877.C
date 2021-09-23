/* PR sanitizer/99877*/
/* { dg-options "-fsanitize=address -fopenmp -O2" } */

struct vector
{
  int size ();
};
int
main ()
{
  vector outqueue;
#pragma omp parallel
  {
    goto continueloop;
  continueloop:;
  }
  for (; outqueue.size ();)
    ;
}
