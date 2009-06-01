/* { dg-options "-I. -Winvalid-pch" } */

#define DEFINED_VALUE 3

#include "warn-1.h"/* { dg-warning "not used because .DEFINED_VALUE. is defined" } */
/* { dg-error "No such file" "no such file" { target *-*-* } 0 } */
/* { dg-error "they were invalid" "invalid files" { target *-*-* } 0 } */
/* { dg-message "terminated" "" { target *-*-* } 0 } */


int main(void) 
{
  return DEFINED_VALUE;
}
