#include "lib/memmove.c"
#ifdef __vxworks
/* The RTP C library uses bzero and bfill, both of which are defined
   in the same file as bcopy.  */
#include "lib/bzero.c"
#include "lib/bfill.c"
#endif
