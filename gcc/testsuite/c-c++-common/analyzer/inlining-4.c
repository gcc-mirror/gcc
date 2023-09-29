/* Verify that we can reconstruct fndecl and stack depth information
   after early inlining.  */

/* { dg-additional-options "-O2 -fdiagnostics-show-path-depths" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

static inline const char*
inner (int flag)
{
  if (flag)
  /* { dg-message "following 'true' branch \\(when 'flag != 0'\\)\\.\\.\\. \\(fndecl 'inner', depth 3\\)" "" { target c } .-1 } */
  /* { dg-message "following 'true' branch \\(when 'flag != 0'\\)\\.\\.\\. \\(fndecl 'const char\\* inner\\(int\\)', depth 3\\)" "" { target c++ } .-2 } */
    return NULL;
  return "foo";
}

static inline const char*
middle (int flag)
{
  return inner (flag);
}

char
outer (int flag)
{
  return *middle (flag); /* { dg-warning "dereference of NULL" "warning" } */
  /* { dg-message "\\(fndecl 'outer', depth 1\\)" "message" { target c } .-1 } */
  /* { dg-message "\\(fndecl 'char outer\\(int\\)', depth 1\\)" "message" { target c++ } .-2 } */
}
