/* { dg-do compile } */
/* { dg-additional-options "-foffload-memory=pinned" } */

#pragma omp requires unified_address        /* { dg-error ".unified_address. is incompatible with the selected .-foffload-memory. option" } */
