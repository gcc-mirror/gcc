// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/pkgmod313/mod.d imports/pkgmod313/package.d
import imports.pkgmod313;

void test()
{
    imports.pkgmod313.foo();
    imports.pkgmod313.bar();
}
