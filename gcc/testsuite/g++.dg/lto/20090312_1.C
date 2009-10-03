#include "20090312.h"

/* This file should be compiled with the C front end.  This
   should be testing what happens when LTO merges enum types and function
   prototypes compiled by the C and C++ FEs.  Since both FEs generate
   slightly different representations for these, LTO was emitting an
   ODR violation error.

   Once dejagnu can deal with multiple languages in a single test, remove
   the __cplusplus checks and force this file to be compiled with the
   C front end.  */
#ifdef __cplusplus
extern "C" {
#endif

JSErrorCallback p = 0;
enum Values x = ONE;

#ifdef __cplusplus
}
#endif
