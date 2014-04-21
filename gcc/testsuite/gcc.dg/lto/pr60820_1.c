#include <stdio.h>
struct in6_addr {int bah;};
extern const struct in6_addr in6addr_any;
static const struct in6_addr local_in6addr_any = {1};
#pragma weak in6addr_any = local_in6addr_any

__attribute__ ((used))
void foo()
{
  fprintf (stderr, "v1: %p, v2: %p\n", &local_in6addr_any, &in6addr_any);
}
