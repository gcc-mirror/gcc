#include "i386/xm-i386.h"

/* malloc does better with chunks the size of a page.  */ 

#define OBSTACK_CHUNK_SIZE (getpagesize ())
