/*
TEST_OUTPUT:
---
fail_compilation/mixin_template.d(10): Error: mixin `mixin_template.f!1` - `f` is a function, not a template
---
*/
string f() {
    return "int i;";
}
mixin f!1;
