#include "xm-m68k.h"

/* malloc does better with chunks the size of a page.  */ 

#define OBSTACK_CHUNK_SIZE (getpagesize ())

/* Avoid warnings when `wait' is passed an `int *'.  */
#define wait(ARG) wait ((union wait *) (ARG))
