/* { dg-additional-options "-D_POSIX_SOURCE" } */

#include <stdio.h>

int test_pass_through (FILE *stream)
{
  return fileno (stream);
}
