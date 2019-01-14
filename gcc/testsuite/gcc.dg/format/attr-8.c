/* Test to verify that parameters of qualified narrow char pointer type
   are accepted for attribute format printf, but others are rejected.
   { dg-do compile }
   { dg-options "-std=gnu99 -Wformat" } */

#define DONT_GNU_PROTOTYPE
#include "format.h"

#define FORMAT(archetype, arg1, arg2)   \
  __attribute__  ((format (archetype, arg1, arg2)))

FORMAT (gnu_attr_printf, 1, 2)
  void fpc_1_2 (char *, ...);

FORMAT (gnu_attr_printf, 1, 2)
  void fpcc_1_2 (const char *, ...);

FORMAT (gnu_attr_printf, 1, 2)
  void frpc_1_2 (char * restrict, ...);

FORMAT (gnu_attr_printf, 1, 2)
  void fpvc_1_2 (volatile char *, ...);

FORMAT (gnu_attr_printf, 1, 2)
  void fpcvc_1_2 (const volatile char *, ...);

FORMAT (gnu_attr_printf, 1, 2)
  void fpv_1_2 (void *, ...);       /* { dg-error ".format. attribute argument 2 value .1. refers to parameter type .void \\\*." } */

FORMAT (gnu_attr_printf, 1, 2)
  void fppc_1_2 (char **, ...);     /* { dg-error ".format. attribute argument 2 value .1. refers to parameter type .char \\\*\\\*." } */

FORMAT (gnu_attr_printf, 1, 2)
  void fpwc_1_2 (wchar_t *, ...);   /* { dg-error ".format. attribute argument 2 value .1. refers to parameter type .wchar_t \\\*." } */
