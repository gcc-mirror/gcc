/* PR libgomp/61200 */

int
main ()
{
  int var = 1;
  #pragma omp parallel
    if (var != 1)
      __builtin_abort ();
  #pragma omp task shared(var)
    var = 2;
  return 0;
}
