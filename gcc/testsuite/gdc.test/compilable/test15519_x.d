// EXTRA_FILES: test15519_y.d
import test15519_y;

extern(C++, ns)
{
 class X { test15519_y.ns.Y v; }
}
