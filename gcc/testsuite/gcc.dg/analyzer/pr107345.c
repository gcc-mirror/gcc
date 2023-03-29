/* Ensure the analyzer treats (NULL == &e) as being false for this case,
   where the logic is sufficiently complicated to not be optimized away.  */

#include <stdio.h>

int main() {   
  int e = 10086;
  int *f = &e;
  int g = 0;
  int *h[2][1];
  h[1][0] = f;
  if (g == (h[1][0])) { /* { dg-warning "comparison between pointer and integer" } */
    unsigned int *i = 0;
  }
  printf("NPD_FLAG: %d\n ", *f);
  return 0;
}
