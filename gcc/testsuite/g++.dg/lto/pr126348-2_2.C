#include "pr126348.h"

__attribute__ ((noinline)) int one (const Buffer &);
__attribute__ ((noinline)) int two (const Buffer &);

int
main (int argc, char **)
{
  Buffer buffer = { argc };
  return one (buffer) + two (buffer);
}
