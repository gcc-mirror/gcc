// first resolved as package, then created as module (with name package)
// COMPILED_IMPORTS: imports/pkgmod313/mod.d imports/pkgmod313/package.d
// REQUIRED_ARGS: -de
import imports.pkgmod313; // then imported as package module

void test()
{
    imports.pkgmod313.foo();
}
