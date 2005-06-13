// PR tree-opt/21994
// { dg-do compile }
// { dg-options "-O2" }

_Complex float f(void);
_Complex float g(void) throw()
{
  return f();
}
