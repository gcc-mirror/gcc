/* Sequent DYNIX/ptx 2.x (SVr3) */

#include "i386/seq-sysv3.h"

/* Use atexit for static destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

