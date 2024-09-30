/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

/* Reproducer for PR analyzer/97258: we should report the double-free
   inside a static callback if the callback escapes.  */

#include <stdlib.h>

static void callback_1 (void *p)
{
  free (p);
  free (p); /* { dg-warning "double-'free' of 'p'" } */
}

struct ops {
  void (*cb) (void *);
};

static const struct ops ops_1 = {
  .cb = callback_1
};

extern void registration (const void *);

void register_1 (void)
{
  registration (&ops_1);
}
