#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern char *strtok (char *str, const char *delim)
  __attribute__((nonnull (2)));

int
main (int argc, char *argv[])
{
  char *cmd;
  char *arg;

  if (argc < 2)
    return -1;

  cmd = strtok (NULL, " "); /* { dg-line "first_call" } */
  arg = strtok (NULL, " ");
  return 0;

  /* C:
     { dg-warning "calling 'strtok' for first time with NULL as argument 1 has undefined behavior \\\[CWE-476\\\] \\\[-Wanalyzer-undefined-behavior-strtok\\\]" "" { target c } first_call }
     { dg-message "some implementations of 'strtok' may crash on such input" "" { target c } first_call } */

  /* C++:
     { dg-warning "calling 'char\\* strtok\\(char\\*, const char\\*\\)' for first time with NULL as argument 1 has undefined behavior \\\[CWE-476\\\] \\\[-Wanalyzer-undefined-behavior-strtok\\\]" "" { target c++ } first_call }
     { dg-message "some implementations of 'char\\* strtok\\(char\\*, const char\\*\\)' may crash on such input" "" { target c++ } first_call }  */
}
