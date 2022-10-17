// EXTRA_FILES: test15519_x.d
import test15519_x: NS = ns;            // fails
//import test15519_x; alias test15519_x.ns NS;  // works

extern(C++, ns)
{
 class Y { NS.X v; }
}
