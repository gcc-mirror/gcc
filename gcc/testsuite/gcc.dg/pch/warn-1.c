/* { dg-options "-I. -Winvalid-pch" } */

#define DEFINED_VALUE 3

#include "warn-1.h"/* { dg-warning "not used because .DEFINED_VALUE. is defined" } */
/* { dg-error "No such file" "no such file" { target *-*-* } 5 } */
/* { dg-error "they were invalid" "invalid files" { target *-*-* } 5 } */


int main(void) 
{
  return DEFINED_VALUE;
}
