/* PR middle-end/94004 - missing -Walloca on calls to alloca due
   to -Wno-system-headers
   { dg-do compile }
   { dg-options "-O2 -Wall -Wvla-larger-than=31 -ftrack-macro-expansion=0" } */

void sink (void*, ...);

static inline void inline_use_vla (unsigned n)
{
  if (n > 32)
    n = 32;
  char a[n];                  // { dg-warning "\\\[-Wvla-larger-than" }
  sink (a, 2);
}

static inline void use_inlined_vla (unsigned n)
{
  inline_use_vla (n);         // this call is okay
  inline_use_vla (n + 1);     // this one is not (line 19)
}

void call_inline (void)
{
  use_inlined_vla (31);       // line 24
}

/* Verify that the inlining context is included and that it points
   to the correct line number in the inlined function:
   { dg-message "function 'inline_use_vla'.*inlined from 'use_inlined_vla'.*:19:.*inlined from 'call_inline' .*:24:" "" { target *-*-* } 0 }  */
