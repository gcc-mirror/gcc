/* { dg-options "-I. -Winvalid-pch -fexceptions" } */

#include "valid-2.h"/* { dg-error "settings for -fexceptions do not match|No such file|they were invalid" } */

int x;
