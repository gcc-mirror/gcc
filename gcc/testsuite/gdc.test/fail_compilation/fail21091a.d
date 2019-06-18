// https://issues.dlang.org/show_bug.cgi?id=21091

/*
TEST_OUTPUT:
----
fail_compilation/fail21091a.d(15): Error: module `Ternary` is in file 'Ternary.d' which cannot be read
import path[0] = fail_compilation
import path[1] = $p:druntime/import$
import path[2] = $p:phobos$
----
*/

struct NullAllocator
{
    import Ternary;
    Ternary owns() { }
}
