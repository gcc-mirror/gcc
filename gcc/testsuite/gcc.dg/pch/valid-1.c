/* { dg-options "-I. -Winvalid-pch -g" } */

#include "valid-1.h"/* { dg-error "created with -gnone, but used with -g|No such file|they were invalid" } */

int x;
