/* Verify diagnostics for aliases to strings containing extended
   identifiers or bad characters.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -w" } */
/* { dg-require-alias "" } */
/* { dg-require-ascii-locale "" } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

void f0 (void) __attribute__((alias("\xa1"))); /* { dg-error "undefined symbol '\\\\241'" } */
void f1 (void) __attribute__((alias("\u00e9"))); /* { dg-error "undefined symbol '\\\\U000000e9'" } */
void f2 (void) __attribute__((alias("\uffff"))); /* { dg-error "undefined symbol '\\\\U0000ffff'" } */
void f3 (void) __attribute__((alias("\U000fffff"))); /* { dg-error "undefined symbol '\\\\U000fffff'" } */
void f4 (void) __attribute__((alias("\U00ffffff"))); /* { dg-error "undefined symbol '\\\\U00ffffff'" } */
void f5 (void) __attribute__((alias("\U0fffffff"))); /* { dg-error "undefined symbol '\\\\U0fffffff'" } */
