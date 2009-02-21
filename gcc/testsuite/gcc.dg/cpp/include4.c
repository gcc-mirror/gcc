/* Preprocessing tokens are always formed according to a greedy algorithm,
   so "#include <stddef.h" must be interpreted as a sequence of tokens,
   of which the "h" then gets macro expanded.  Likewise the other
   examples.  */

#define h h>
#include <stddef.h
#undef h

#define foo stddef.h>
#include <foo

#include <foo /*
> */
