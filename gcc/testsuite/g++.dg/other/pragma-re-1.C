/* { dg-final { scan-assembler "bar" } } */
/* { dg-final { scan-assembler-not "foo" } } */
/* { dg-final { scan-assembler "_Z3bazv" } } */
/* { dg-final { scan-assembler-not "baq" } } */
/* { dg-final { scan-assembler "tut" } } */
/* { dg-final { scan-assembler-not "gee" } } */
/* { dg-final { scan-assembler "bang" } } */
/* { dg-final { scan-assembler-not "whiz" } } */
/* { dg-final { scan-assembler "eek" } } */
/* { dg-final { scan-assembler-not "boo" } } */

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

// PR c++/30112
// These are expected to work.
#pragma redefine_extname gee tut
namespace somewhere {
  extern "C" int gee(void);
  int (*r)(void) = gee;

  extern "C" int whiz(void);
  int whiz(int);
}
#pragma redefine_extname whiz bang
int (*s)() = somewhere::whiz;

namespace elsewhere {
  extern "C" int whiz(void);
}
int (*t)() = elsewhere::whiz;

namespace A
{
  extern "C" int boo(void);
}

namespace B
{
  extern "C" int boo(void);
}
#pragma redefine_extname boo eek

int (*u)() = A::boo;
int (*v)() = B::boo;
