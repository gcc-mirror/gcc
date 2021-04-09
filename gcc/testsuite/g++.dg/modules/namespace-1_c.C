// { dg-additional-options "-fmodules-ts" }
// The indirect import of frob, with namespaces impl and ompl doesn't
// affect us.
static int impl;
import Frink;

static int ompl;

void corge (int x)
{
  impl = x;
  ompl = frab (x);
}
