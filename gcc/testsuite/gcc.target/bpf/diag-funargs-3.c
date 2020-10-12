/* Verify proper errors are generated for functions taking too many
   arguments, with aggregates and 128-bit arguments.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

#include <stdint.h>

struct ja
{
  long i1;
  long i2;
  long i3;
  long i4;
  long i5;
  long i6;
};

void jorl (struct ja, int, int, int, unsigned __int128);

int foo ()
{
  struct ja je;
  jorl (je, 1, 2, 3, 4); /* { dg-error "too many function arguments" } */
  return 2L /1;
}
