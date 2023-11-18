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
      for (; i >= 0; i--) {
	free(arr[i]); /* { dg-bogus "double-'free'" } */
      }
      free(arr); /* { dg-bogus "leak" "" { xfail *-*-* } } */
      return NULL;
    }
  }
  return arr;
}
