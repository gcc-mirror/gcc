/*
TEST_OUTPUT:
---
fail_compilation/issue16020.d(14): Error: user-defined attributes not allowed for `alias` declarations
fail_compilation/issue16020.d(15): Error: semicolon expected to close `alias` declaration, not `(`
fail_compilation/issue16020.d(15): Error: semicolon needed to end declaration of `t` instead of `)`
fail_compilation/issue16020.d(15): Error: declaration expected, not `)`
fail_compilation/issue16020.d(16): Deprecation: storage class `final` has no effect in type aliases
---
*/
module issue16020;

struct UDA{}
alias Fun = @UDA void();
alias FunTemplate = void(T)(T t);
alias F2 = final int();
