#pragma omp requires reverse_offload

void
foo ()
{
  /* Good nesting - as reverse offload */
  #pragma omp target
   #pragma omp target device(ancestor:1)  /* valid -> no warning */   /* { dg-bogus "'target' construct inside of 'target' region" }  */
    { }

  /* Bad nesting */
  #pragma omp target
   #pragma omp target  /* { dg-warning "'target' construct inside of 'target' region" }  */
     #pragma omp target  /* { dg-warning "'target' construct inside of 'target' region" }  */
    { }

  /* Good nesting - as reverse offload */
  #pragma omp target
   #pragma omp target  /* { dg-warning "'target' construct inside of 'target' region" }  */
     #pragma omp target device(ancestor:1)  /* valid -> no warning */   /* { dg-bogus "'target' construct inside of 'target' region" }  */
      { }

  #pragma omp target
   #pragma omp target device(ancestor:1)  /* valid -> no warning */   /* { dg-bogus "'target' construct inside of 'target' region" }  */
     #pragma omp target device(ancestor:1) /* { dg-error "OpenMP constructs are not allowed in target region with 'ancestor'" }  */
       { }

}
