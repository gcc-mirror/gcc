extern "C" void abort (void);

#include "empty6.h"

void empty6_y (B& b)
{
  if (b.i != 7)
    abort ();
}
