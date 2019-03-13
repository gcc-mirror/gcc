// { dg-additional-options -fmodules-ts }
export module quux;
// { dg-module-bmi quux }
import frob;

export int foo (frob *p)
{
  return p->m;
}
