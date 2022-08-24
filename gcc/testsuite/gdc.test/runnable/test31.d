// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/test31a.d
// PERMUTE_ARGS:

import imports.test31a;

class Foo {
    mixin Baz!();

    void testfunc() {
        privfunc(); // Error: .privfunc is private
    }
}

int main()
{
    return 0;
}
