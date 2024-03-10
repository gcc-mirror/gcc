/* TODO: enable for C++ once implemented. */
/* { dg-do compile { target c } } */

#pragma omp requires dynamic_allocators

#pragma omp begin declare target
void
f ()
{

  int var;  /* { dg-message "sorry, unimplemented: OpenMP 'allocate' directive, used for 'var', not yet supported" } */
  #pragma omp allocate(var)
  var = 5;
}
#pragma omp end declare target

void
h ()
{
  #pragma omp target
   #pragma omp parallel
    #pragma omp serial
     {
       int var2[5];  /* { dg-message "sorry, unimplemented: OpenMP 'allocate' directive, used for 'var2', not yet supported" } */
       #pragma omp allocate(var2)
       var2[0] = 7;
     }
}
