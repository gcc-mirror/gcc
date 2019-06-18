// EXTRA_FILES: test15389_y.d
import test15389_y;

//struct ns
extern (C++, ns)
{
    class X { test15389_y.ns.Y a; }
}
