/*
REQUIRED_ARGS: -Icompilable/imports
EXTRA_SOURCES: imports/test19657b.d imports/test19657c.d imports/test19657d.d imports/test19657e.d imports/test19657f.d imports/test19657g.d
*/

import test19657c;
import test19657e: Bar;
class Foo {
  int[Foo] _map;
  bool func (Foo rhs, Bar bee) { return true; }
}
