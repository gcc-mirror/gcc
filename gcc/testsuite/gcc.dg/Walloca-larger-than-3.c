/* PR middle-end/94004 - missing -Walloca on calls to alloca due
   to -Wno-system-headers
   { dg-do compile }
   { dg-options "-O2 -Wall -Walloca-larger-than=8 -ftrack-macro-expansion=0" } */

#include "Walloca-larger-than-3.h"

void sink (void*, ...);

void call_builtin_alloca (int n)
{
  if (n < 9)
    n = 9;
  void *p = __builtin_alloca (n);   // { dg-warning "\\\[-Walloca-larger-than" }
  sink (p, 0);
}

void call_alloca_sys_hdr (int n)
{
  if (n < 9)
    n = 9;
  void *p = alloca (n);             // { dg-warning "\\\[-Walloca-larger-than" }
  sink (p, 1);
}

static inline void inline_call_alloca (int n)
{
  if (n > 9)
    n = 9;
  void *p = alloca (n);             // { dg-warning "\\\[-Walloca-larger-than" }
  sink (p, 2);
}

void make_inlined_call (void)
{
  inline_call_alloca (10);
}
