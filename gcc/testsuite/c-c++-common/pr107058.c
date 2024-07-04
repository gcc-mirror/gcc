/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-g -fdebug-types-section -flto -fno-checking" } */

/* We should handle the C++ FE issue gracefully with -fno-checking.  */

#include "pr50459.c"
