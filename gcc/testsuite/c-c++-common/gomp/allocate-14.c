/* TODO: enable for C++ once implemented. */
/* { dg-do compile { target c } } */

#pragma omp begin declare target
void
f ()
{

  int var;  /* { dg-error "'allocate' directive for 'var' inside a target region must specify an 'allocator' clause" } */
  #pragma omp allocate(var)
  var = 5;
}
#pragma omp end declare target

void
h ()
{
  #pragma omp target
   #pragma omp parallel
    #pragma omp single
     {
       int var2[5];  /* { dg-error "'allocate' directive for 'var2' inside a target region must specify an 'allocator' clause" } */
       #pragma omp allocate(var2)
       var2[0] = 7;
     }
}
