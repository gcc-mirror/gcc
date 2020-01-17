#include <stdlib.h>
#include "malloc-ipa-8-lto.h"

void *wrapped_malloc (size_t size)
{
  return malloc (size);
}

void wrapped_free (void *ptr)
{
  free (ptr);
}
