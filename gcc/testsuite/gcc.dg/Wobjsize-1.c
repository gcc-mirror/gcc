/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-array-bounds" } */

#include "Wobjsize-1.h"

char buf[6];

int main(int argc, char **argv)
{
  strcpy (buf,"hello ");    /* { dg-warning "\\\[-Wstringop-overflow" } */
  return 0;
}

/* { dg-message "file included" "included" { target *-*-* } 0 }
   { dg-message "inlined from" "inlined" { target *-*-* } 0 }

   The test might emit two warnings, one for the strcpy call and
   another for the inlined call to __builtin___strcpy_chk() called
   from strcpy().
   { dg-prune-output "writing 7 bytes into a region of size 6" } */
