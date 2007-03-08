#include "lib/strrchr.c"
#ifdef __vxworks
/* The RTP C library uses bzero, bfill and bcopy, all of which are defined
   in the same file as rindex.  */
#include "lib/bzero.c"
#include "lib/bfill.c"
#include "lib/memmove.c"
#endif
