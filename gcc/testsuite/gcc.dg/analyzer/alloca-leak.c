#include <alloca.h>

void *test (void)
{
  void *ptr = alloca (64);
  return ptr;
}
/* TODO: warn about escaping alloca.  */
