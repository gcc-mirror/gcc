#include "i386/xm-i386.h"

/* Inhibit cccp.c's definition of putenv.  */
#define HAVE_PUTENV

/* Use semicolons to separate elements of a path.  */
#define PATH_SEPARATOR ';'

/* Use backslashs to separate levels of directory.  */
#define DIR_SEPARATOR '\\'

/* Suffix for executable file names.  */
#define EXECUTABLE_SUFFIX ".exe"

#define MKTEMP_EACH_FILE 1

#define NO_PRECOMPILES 1
