/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

/* Reproducer for PR analyzer/97258: we should report the double-free
   inside a static callback if the callback is accessible via a global
   initializer.  */

#include <stdlib.h>

static void callback_1 (void *p)
{
  free (p);
  free (p); /* { dg-warning "double-'free' of 'p'" } */
}

struct ops {
  void (*cb) (void *);
};

/* Callback struct is not static, and so could be accessed via
   another TU.  */

const struct ops ops_1 = {
  .cb = callback_1
};
