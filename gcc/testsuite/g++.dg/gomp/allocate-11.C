/* Just a silly ICE I came across by accident, easy fix, might be a problem
   with lookup_name but I'm not certain.

   For some reason, the floating 'a;' breaks lookup_name in tsubst_stmt during
   substitution of the allocate directive, despite it being found no problem
   during parsing of the allocate directive's var list.
   I don't have time to investigate it further so I'm just going to fix it
   by checking for NULL_TREE on the return of lookup_name.  */
   
template<typename>
void f()
{
    a; /* { dg-error "'a' was not declared in this scope" } */
    int a = 42;
    #pragma omp allocate(a)
}
template void f<void>();

