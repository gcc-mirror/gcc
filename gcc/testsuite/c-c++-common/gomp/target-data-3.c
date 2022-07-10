/* In OpenMP 5.2 permits tofrom for enter/exit data
   in the FE, it is already converted to 'to' and 'from', respectively. */
int y, z;

void
copyin ()
{
  #pragma omp target enter data map(from: y)         /* { dg-error "'#pragma omp target enter data' with map-type other than 'to', 'tofrom' or 'alloc' on 'map' clause" } */
  #pragma omp target enter data map(always, from: z) /* { dg-error "'#pragma omp target enter data' with map-type other than 'to', 'tofrom' or 'alloc' on 'map' clause" } */
}

void
copyout ()
{
  #pragma omp target exit data map(to: y)         /* { dg-error "'#pragma omp target exit data' with map-type other than 'from', 'tofrom', 'release' or 'delete' on 'map' clause" } */
  #pragma omp target exit data map(always, to: z) /* { dg-error "'#pragma omp target exit data' with map-type other than 'from', 'tofrom', 'release' or 'delete' on 'map' clause" } */
}
