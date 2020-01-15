#include <stdlib.h>

#include "double-free-lto-1.h"

extern void calls_free (void *ptr)
{
  free (ptr); 
}
