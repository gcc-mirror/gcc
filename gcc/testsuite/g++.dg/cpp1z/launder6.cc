#include "launder6.h"

void *
operator new (decltype (sizeof (0)), void *p)
{
  return p;
}

int y = 666;

void f(B& b)
{
  new (&b.a) A{y};
}
