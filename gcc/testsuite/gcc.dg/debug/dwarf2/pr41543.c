/* PR preprocessor/41543 */
/* { dg-do compile } */
/* { dg-options "-save-temps -gdwarf -O0 -dA -fno-merge-debug-strings" } */

#include <stdarg.h>

int
foo (va_list ap)
{
  return va_arg (ap, int);
}

/* { dg-final { scan-assembler-not "DW_AT_decl_file\[^\\r\\n\]*\(pr41543\.i\)" } } */
