#include "xm-i386.h"

/* Inhibit cccp.c's definition of putenv.  */
#define HAVE_PUTENV

/* Use semicolons to separate elements of a path.  */
#define PATH_SEPARATOR ';'

/* Suffix for executable file names.  */
#define EXECUTABLE_SUFFIX ".exe"
