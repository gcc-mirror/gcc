/* Test __builtin_FILE(). */
/* { dg-do run } */
/* { dg-options "-ffile-prefix-map==FILE-PREFIX" } */

#include <stdio.h>

int main ()
{
  printf ("__builtin_FILE starts with %s\n", __builtin_FILE ());
}

/* { dg-output "__builtin_FILE starts with FILE-PREFIX" } */
