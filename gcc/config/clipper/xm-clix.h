/* Config file for Clipper running Clix, system V. 3.2 clone  */


/* #defines that need visibility everywhere.  */
#define FALSE 0
#define TRUE 1

/* target machine dependencies.
   tm.h is a symbolic link to the actual target specific file.   */

#include "tm.h"

/* This describes the machine the compiler is hosted on.  */
#define HOST_BITS_PER_CHAR 8
#define HOST_BITS_PER_SHORT 16
#define HOST_BITS_PER_INT 32
#define HOST_BITS_PER_LONG 32
#define HOST_BITS_PER_LONGLONG 64

/* This machine uses IEEE floats.  */
/* #define HOST_FLOAT_FORMAT IEEE_FLOAT_FORMAT */

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* isinf isn't there, but finite is. */
#define isinf(x) (!finite(x))


#define USG

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)
#define index strchr
#define rindex strrchr

#define TARGET_MEM_FUNCTIONS
