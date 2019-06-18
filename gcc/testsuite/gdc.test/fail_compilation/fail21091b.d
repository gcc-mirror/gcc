// https://issues.dlang.org/show_bug.cgi?id=21091

/*
TEST_OUTPUT:
----
fail_compilation/fail21091b.d(15): Error: module `Tid` is in file 'Tid.d' which cannot be read
import path[0] = fail_compilation
import path[1] = $p:druntime/import$
import path[2] = $p:phobos$
----
*/

class Logger
{
    import Tid;
    Tid threadId;
}
