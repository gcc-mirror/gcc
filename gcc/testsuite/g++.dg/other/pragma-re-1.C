/* { dg-do compile { target *-*-solaris* } } */
/* { dg-final { scan-assembler "bar" } } */
/* { dg-final { scan-assembler-not "foo" } } */
/* { dg-final { scan-assembler "_Z3bazv" } } */
/* { dg-final { scan-assembler-not "baq" } } */

#ifndef __PRAGMA_REDEFINE_EXTNAME
#error 
#endif

/* This one is expected to work.  */
#pragma redefine_extname foo bar
extern "C" int foo(void);
int (*p)(void) = foo;

/* This one is expected not to work (redefine_extname
   can only be applied to extern "C" names).  */
#pragma redefine_extname baz baq
extern int baz(void);
int (*q)(void) = baz;
