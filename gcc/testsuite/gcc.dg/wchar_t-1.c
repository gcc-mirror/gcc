/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* Compile with -Wall to get a warning if built-in and system wchar_t don't
   match.  */

#define _STDDEF_H
#include <wchar.h>

__WCHAR_TYPE__ __wc_t__;
wchar_t *wc_t_p;

void
wct (void)
{
  wc_t_p = &__wc_t__;
}
