/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

#include "Wobjsize-1.h"

char buf[6];
int main(int argc, char **argv)
{
  strcpy (buf,"hello ");
  return 0;
}

/* { dg-warning "writing" "" { target *-*-* } 6 } */
/* { dg-message "file included" "included" { target *-*-* } 0 } */
/* { dg-message "inlined from" "inlined" { target *-*-* } 0 } */
