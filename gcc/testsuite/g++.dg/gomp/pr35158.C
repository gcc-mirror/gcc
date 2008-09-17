// PR c++/35158
// { dg-do compile }
// { dg-options "-fopenmp" }

int main(int argc, char *argv[])
{
#pragma omp parallel for
  for (int i(0) ; // { dg-error "parenthesized initialization is not allowed in OpenMP 'for' loop" }
       i < 10 ; 
       i++) 
    ;
  
  return 0;
}
