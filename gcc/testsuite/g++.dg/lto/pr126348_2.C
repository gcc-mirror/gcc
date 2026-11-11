#include "pr126348.h"

__attribute__ ((noinline)) int one (const Buffer &);
__attribute__ ((noinline)) int one_extra (const Buffer &);
__attribute__ ((noinline)) int two (const Buffer &);
__attribute__ ((noinline)) int two_extra (const Buffer &);

int
main (int argc, char **)
{
  Buffer buffer = { argc };
  return one (buffer) + one_extra (buffer)
	 + two (buffer) + two_extra (buffer);
}
