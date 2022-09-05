/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

  /* Test to ensure that device-modifier 'ancestor' is parsed correctly in
     device clauses. */

#pragma omp requires reverse_offload

void
foo (void)
{
  #pragma omp target device (ancestor: 1)
  ;

}

/* { dg-final { scan-tree-dump "pragma omp target \[^\n\r)]*device\\(ancestor:1\\)" "original" } } */
