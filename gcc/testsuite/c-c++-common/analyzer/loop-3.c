/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void test(int c)
{
  int i;
  char *buffer = (char*)malloc(256);

  for (i=0; i<255; i++) {
    buffer[i] = c; /* { dg-warning "use after 'free' of 'buffer'" "use after free" { xfail *-*-* } } */
                   /* { dg-warning "possibly-NULL 'buffer'" "deref of unchecked" { target *-*-* } .-1 } */
    free(buffer); /* { dg-warning "double-'free' of 'buffer'" } */
  }

}
