/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#include <stdlib.h>

struct foo { int dummy; };

struct foo **
test (int n) {
  struct foo **arr;
  int i;

  if ((arr = (struct foo **)malloc(n * sizeof(struct foo *))) == NULL)
    return NULL;

  for (i = 0; i < n; i++) {
    if ((arr[i] = (struct foo *)malloc(sizeof(struct foo))) == NULL) {
      for (; i >= 0; i++) { /* { dg-warning "infinite loop" } */
	/* This loop is in the wrong direction, so not technically an
	   infinite loop ("i" will eventually wrap around), but the
	   analyzer's condition handling treats the overflow as such.
	   In any case, the code is suspect and warrants a warning.  */
	free(arr[i]); /* { dg-bogus "double-'free'" } */
      }
      free(arr); /* { dg-warning "leak" } */
      return NULL;
    }
  }
  return arr;
}
