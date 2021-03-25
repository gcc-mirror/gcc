#include "analyzer-decls.h"

/* The example from store.h  */

void test_1 (char a, char b, char c, char d, char e, char f,
	     int i, int j)
{
  char arr[1024];
  arr[2] = a;  /* (1) */
  arr[3] = b;  /* (2) */

  __analyzer_eval (arr[2] == a); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[3] == b); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[4]); /* { dg-warning "UNKNOWN" } */ // TODO: report uninit

  /* Replace one concrete binding's value with a different value.  */
  arr[3] = c;  /* (3) */
  __analyzer_eval (arr[2] == a); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[3] == c); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[3] == b); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (arr[4]); /* { dg-warning "UNKNOWN" } */ // TODO: report uninit

  /* Symbolic binding.  */
  arr[i] = d;  /* (4) */
  __analyzer_eval (arr[i] == d); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[2] == a); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (arr[3] == c); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (arr[4]); /* { dg-warning "UNKNOWN" } */ /* Don't report uninit. */

  /* Replace symbolic binding with a different one.  */
  arr[j] = e;  /* (5) */
  __analyzer_eval (arr[j] == e); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[i] == d); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (arr[4]); /* { dg-warning "UNKNOWN" } */ /* Don't report uninit. */

  /* Add a concrete binding.  */
  arr[3] = f;  /* (6) */
  __analyzer_eval (arr[3] == f); /* { dg-warning "TRUE" } */
  __analyzer_eval (arr[j] == e); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (arr[4]); /* { dg-warning "UNKNOWN" } */ /* Don't report uninit. */
}

// TODO: as above, but with int rather than char so there's a cast
