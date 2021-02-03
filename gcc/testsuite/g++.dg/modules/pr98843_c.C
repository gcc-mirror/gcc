// { dg-additional-options -fmodules-ts }
// PR 98843 ICE due to inconsistent entity_ary order

import foo;
import "pr98843_b.H";

int main ()
{
  return Fn<1> ();
}
