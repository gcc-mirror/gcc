/* { dg-do compile { target *-*-solaris* } } */
/* { dg-final { scan-assembler "bar" } } */
/* { dg-final { scan-assembler-not "foo" } } */
/* { dg-final { scan-assembler "_Z3bazv" } } */
/* { dg-final { scan-assembler-not "baq" } } */

#ifndef __PRAGMA_REDEFINE_EXTNAME
#error 
#endif

#pragma redefine_extname foo bar
extern "C" int foo(void);
void *p = (void *)foo;

#pragma redefine_extname baz baq
extern int baz(void);
void *q = (void *)baz;
