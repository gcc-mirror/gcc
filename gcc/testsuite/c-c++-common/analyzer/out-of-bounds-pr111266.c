#include <stdint.h>
void *malloc (__SIZE_TYPE__);
void free (void *);

void test_binop2 ()
{
  char *p = (char *) malloc (4);
  int32_t *i = (int32_t *) (p + 3);
  *i = 20042; /* { dg-warning "heap-based buffer overflow" } */
  free (p);
}
