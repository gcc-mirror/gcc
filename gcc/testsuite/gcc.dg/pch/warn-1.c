/* { dg-options "-I. -Winvalid-pch" } */

#define DEFINED_VALUE 3

#include "warn-1.h"/* { dg-error "not used because `DEFINED_VALUE' is defined|No such file|they were invalid" } */

int main(void) 
{
  return DEFINED_VALUE;
}
