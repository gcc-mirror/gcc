/* { dg-do compile } */

void
foo (void)
{
  /* Ensure that a 'requires' directive with the 'reverse_offload' clause was
     specified.  */

  #pragma omp target device (ancestor : 1) /* { dg-error "'ancestor' device modifier not preceded by 'requires' directive with 'reverse_offload' clause" } */
    /* { dg-error "expected '\\)' before 'ancestor'" "" { target c } .-1 } */

  ;
}
