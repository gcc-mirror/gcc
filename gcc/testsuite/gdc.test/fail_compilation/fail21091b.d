// https://issues.dlang.org/show_bug.cgi?id=21091

/*
TEST_OUTPUT:
----
fail_compilation/fail21091b.d(16): Error: unable to read module `Tid`
fail_compilation/fail21091b.d(16):        Expected 'Tid.d' or 'Tid/package.d' in one of the following import paths:
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
