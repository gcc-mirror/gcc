// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Mar 2005 <nathan@codesourcery.com>

// Origin: Giovanni Bajo <giovannibajo@libero.it>
// Bug 19203: Failure to implement DR 214

template <class A>
void foo(const A& a);

template <class RET, class ARG1>
int foo(RET (&)(ARG1)); // this one


float decl(int);

int bar(void)
{
  return foo(decl);
}
