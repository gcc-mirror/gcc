/* PR tree-optimization/86428 - strlen of const array initialized with
   a string of the same length not folded
   { dg-do compile }
   { dg-options "-O0 -Wall -fdump-tree-gimple" } */

#include "strlenopt.h"

const char a1[1] = "\0";
const char a2[2] = "1\0";
const char a3[3] = "12\0";
const char a8[8] = "1234567\0";
const char a9[9] = "12345678\0";

const char ax[9] = "12345678\0\0\0\0";   /* { dg-warning "initializer-string for array of 'char' is too long" } */
const char ay[9] = "\00012345678\0\0\0\0";   /* { dg-warning "initializer-string for array of 'char' is too long" } */


int len1 (void)
{
  size_t len0 = strlen (a1);
  return len0;
}

int len (void)
{
  size_t len = strlen (a2) + strlen (a3) + strlen (a8) + strlen (a9);
  return len;
}

int lenx (void)
{
  size_t lenx = strlen (ax);
  return lenx;
}

int leny (void)
{
  size_t leny = strlen (ay);
  return leny;
}

int cmp88 (void)
{
  int cmp88 = memcmp (a8, "1234567\0", sizeof a8);
  return cmp88;
}

/* { dg-final { scan-tree-dump-times "strlen1" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "len0 = 0;" 1 "gimple" } }
   { dg-final { scan-tree-dump-times "len = 18;" 1 "gimple" } }
   { dg-final { scan-tree-dump-times "lenx = 8;" 1 "gimple" } }
   { dg-final { scan-tree-dump-times "leny = 0;" 1 "gimple" } }
   { dg-final { scan-tree-dump-times "cmp88 = 0;" 1 "gimple" } } */
