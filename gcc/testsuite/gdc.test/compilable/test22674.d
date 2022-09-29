// https://issues.dlang.org/show_bug.cgi?id=22674
// EXTRA_FILES: imports/cimports2a.i imports/cimports2b.i

import imports.cimports2a;
import imports.cimports2b;

void do_foo(){
    FooRef f = make_foo(); // use_foo.d(5)
    free_foo(f);           // use_foo.d(6)
}
