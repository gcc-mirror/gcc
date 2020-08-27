#include <stdlib.h>

void *ptr;

void *test (void)
{
  ptr = malloc (1024);
  ptr = NULL; /* { dg-warning "leak of 'ptr'" } */
}
