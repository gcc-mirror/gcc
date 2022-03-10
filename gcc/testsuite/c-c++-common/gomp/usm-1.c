/* { dg-do compile } */
/* { dg-additional-options "-foffload-memory=pinned" } */

#pragma omp requires unified_shared_memory  /* { dg-error ".unified_shared_memory. is incompatible with the selected .-foffload-memory. option" } */
