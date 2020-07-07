/* { dg-additional-options -fopenmp }  */

/* { dg-error "expected" "" { target *-*-* } 6 } */
/* Make sure we see pragma_eol even though lacking new line.  *
/* no newline at end of file.  */
#pragma omp parallel