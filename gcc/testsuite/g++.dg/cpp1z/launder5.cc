#include "launder5.h"

void *
operator new (decltype (sizeof (0)), void *p)
{
  return p;
}

void f(B& b)
{
  new (&b.a) A{666};
}
