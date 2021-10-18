/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

  /* Test to ensure that device-modifier 'ancestor' is parsed correctly in
     device clauses. */

#pragma omp requires reverse_offload /* { dg-message "sorry, unimplemented: 'reverse_offload' clause on 'requires' directive not supported yet" } */

void
foo (void)
{
  #pragma omp target device (ancestor: 1) /* { dg-message "" "sorry, unimplemented: 'ancestor' not yet supported" { xfail *-*-* } } */
  ;

}

/* { dg-final { scan-tree-dump "pragma omp target \[^\n\r)]*device\\(ancestor:1\\)" "original" } } */
