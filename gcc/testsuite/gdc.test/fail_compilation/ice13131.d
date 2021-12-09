// EXTRA_SOURCES: imports/a13131parameters.d imports/a13131elec.d
// EXTRA_FILES: imports/a13131checkpoint.d
/*
TEST_OUTPUT:
---
+A
+B
fail_compilation/imports/a13131elec.d(10): Error: template instance `elecConnOf!gconn` template `elecConnOf` is not defined
-B
-A
---
*/

void main()
{
    struct Connectivity {}
    auto L = Connectivity();

    import imports.a13131elec;          // [1] import
    L.initElec;
}
