# 1 "defined-syshdr.c" 1
/* { dg-do preprocess } */
/* { dg-options "-Wexpansion-to-defined" } */

#define defined_outside_sys_hdr   defined FOO

# 36 "some-system-header.h" 1 3

#define defined_inside_sys_hdr    defined FOO

/* In a system header, it's acceptable.  */
#if defined_outside_sys_hdr   /* { dg-bogus "may not be portable" } */
#endif

# 16 "defined-syshdr.c" 2

/* Back to _not_ in a system header */

#if defined_outside_sys_hdr   /* { dg-message "may not be portable" } */
#endif

/* Currently we warn about this, but it is probably a good idea not to.  */

#if defined_inside_sys_hdr   /* { dg-bogus "may not be portable" "" { xfail *-*-* } } */
#endif

int x;
