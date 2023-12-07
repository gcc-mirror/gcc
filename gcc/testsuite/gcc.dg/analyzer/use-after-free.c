/* { dg-additional-options "-Wno-analyzer-too-complex -Wno-analyzer-symbol-too-complex" } */

#include <stdlib.h>
#include "analyzer-decls.h"

struct link { struct link *next; };

int free_a_list_badly (struct link *n)
{
  while (n) {
    free(n); /* { dg-message "freed here" } */
    n = n->next; /* { dg-warning "use after 'free' of 'n'" } */
  }
}
