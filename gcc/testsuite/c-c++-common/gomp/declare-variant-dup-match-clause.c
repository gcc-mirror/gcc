/* Check that errors are detected if a match clause appears more than once, 
   even if compatible.  */

void f();
#pragma omp declare variant(f) match(construct={target}) match(construct={target})  /* { dg-error "too many 'match' clauses" } */
void g(); 
