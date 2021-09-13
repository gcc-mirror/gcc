#include <stdlib.h>

struct marker {
  struct marker *next;
  void *ref;
};
struct data {
  struct marker *marker;
};

void data_free(struct data d)
{
  struct marker *nm, *m;

  m = d.marker;
  while (m) {
    nm = m->next;
    free(m->ref);
    free(m);
    m = nm;
  }
}
