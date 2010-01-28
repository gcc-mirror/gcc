/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

#include "Wobjsize-1.h"

char buf[6];
int main(int argc, char **argv)
{
  strcpy (buf,"hello ");
  return 0;
}

/* { dg-warning "will always overflow destination buffer" "" { target *-*-* } 6 } */
/* { dg-message "file included" "" { target *-*-* } 0 } */
/* { dg-message "inlined from" "" { target *-*-* } 0 } */
