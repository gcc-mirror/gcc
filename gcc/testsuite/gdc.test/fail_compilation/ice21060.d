/*
EXTRA_FILES: imports/ice21060a/package.d imports/ice21060b/package.d imports/ice21060c/package.d imports/ice21060d/package.d
TEST_OUTPUT:
---
fail_compilation/imports/ice21060b/package.d(3): Error: struct `imports.ice21060d.P21060` already exists at fail_compilation/imports/ice21060d/package.d(3). Perhaps in another function with the same name?
---
*/
struct S21060
{
    void print()
    {
        import imports.ice21060a;
        import imports.ice21060b;
    }
}
