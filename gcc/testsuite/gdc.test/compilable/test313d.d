// first imported as package
// EXTRA_SOURCES: imports/pkgmod313/mod.d
// REQUIRED_ARGS: -de
import imports.pkgmod313; // then as package module

void test()
{
    imports.pkgmod313.foo();
}
