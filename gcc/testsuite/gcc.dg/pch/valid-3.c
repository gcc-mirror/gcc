/* { dg-options "-I. -Winvalid-pch -fno-unit-at-a-time" } */

#include "valid-3.h"/* { dg-error "settings for -funit-at-a-time do not match|No such file|they were invalid" } */

int x;
