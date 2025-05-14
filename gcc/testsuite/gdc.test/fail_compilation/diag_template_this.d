/**
TEST_OUTPUT:
---
fail_compilation/diag_template_this.d(1): Error: identifier expected for template `this` parameter
fail_compilation/diag_template_this.d(1): Error: found `this` when expecting `(`
fail_compilation/diag_template_this.d(1): Error: semicolon expected following function declaration, not `(`
fail_compilation/diag_template_this.d(1): Error: declaration expected, not `)`
---
 */
#line 1
void func1(this this)() {}
