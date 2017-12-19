/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-array-bounds" } */

#include "Wobjsize-1.h"

char buf[6];
/* { dg-warning "writing" "" { target *-*-* } .-1 } */

int main(int argc, char **argv)
{
  strcpy (buf,"hello ");
  return 0;
}

/* { dg-message "file included" "included" { target *-*-* } 0 } */
/* { dg-message "inlined from" "inlined" { target *-*-* } 0 } */
