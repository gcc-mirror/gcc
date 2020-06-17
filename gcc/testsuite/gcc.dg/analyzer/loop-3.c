#include <stdlib.h>

void test(int c)
{
  int i;
  char *buffer = (char*)malloc(256);

  for (i=0; i<255; i++) {
    buffer[i] = c; /* { dg-warning "use after 'free' of 'buffer'" } */
		   /* BUG: the malloc could have failed
		      TODO: the checker doesn't yet pick up on this, perhaps
		      due to the pointer arithmetic not picking up on the
		      state */
    free(buffer); /* { dg-warning "double-'free' of 'buffer'" } */
  }

}
