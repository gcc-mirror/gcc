/* { dg-options "-I. -save-temps" } */
#include "save-temps-1.h"
#ifndef T
#error T not defined
#endif
#include <stddef.h>
int x;

/* { dg-final { cleanup-saved-temps } } */
