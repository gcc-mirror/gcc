// https://issues.dlang.org/show_bug.cgi?id=22674
// EXTRA_FILES: imports/cimports2a.i imports/cimports2b.i

import imports.cimports2a;
import imports.cimports2b;

void do_foo(){
    FooRef f = make_foo(); // use_foo.d(5)
    free_foo(f);           // use_foo.d(6)
}

// https://issues.dlang.org/show_bug.cgi?id=23357

void do_foo2(){
    FooRef2 f = make_foo2();
    free_foo2(f);
}
