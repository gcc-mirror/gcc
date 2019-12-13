/* Verify that the location information for clauses is correct. */

void
check_clause_columns() {
  int i, j, sum, diff;

  #pragma acc parallel
  {
    #pragma acc loop reduction(+:sum)
    for (i = 1; i <= 10; i++)
      {
        #pragma acc loop reduction(-:diff) reduction(-:sum)
	/* { dg-warning "53: conflicting reduction operations for .sum." "" { target c } .-1 } */
	/* { dg-warning "56: conflicting reduction operations for .sum." "" { target c++ } .-2 } */
	for (j = 1; j <= 10; j++)
	  sum = 1;
      }
  }
}
