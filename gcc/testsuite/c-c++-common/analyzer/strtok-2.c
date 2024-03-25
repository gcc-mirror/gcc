/* { dg-additional-options "-Wno-analyzer-symbol-too-complex" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern char *strtok (char *str, const char *delim)
  __attribute__((nonnull (2)));

int
main(int argc, char *argv[])
{
  char *cmd;
  char *arg;

  if (argc < 2)
    return -1;

  cmd = strtok (argv[1], " "); /* { dg-bogus "undefined behavior" } */
  arg = strtok (NULL, " ");
  return 0;  
}
