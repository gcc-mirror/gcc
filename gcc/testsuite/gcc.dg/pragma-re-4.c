/* Copyright (C) 2006 Free Software Foundation, Inc. */
/* Contributed by Carlos O'Donell on 2006-01-27 */

/* Origin: Carlos O'Donell <carlos@codesourcery.com> */
/* { dg-do compile { target *-*-solaris* } } */
/* { dg-final { scan-assembler "_foo" } } */
/* { dg-final { scan-assembler-not "_foo64" } } */

#define foo     _foo
#define foo64   _foo64
extern int foo(void);
extern int foo64(void);
#pragma redefine_extname foo64 foo
int
bar()
{
        return (foo64());
}
