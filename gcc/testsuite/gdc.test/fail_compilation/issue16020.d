/*
TEST_OUTPUT:
---
fail_compilation/issue16020.d(12): Error: user-defined attributes not allowed for `alias` declarations
fail_compilation/issue16020.d(13): Error: semicolon expected to close `alias` declaration, not `(`
fail_compilation/issue16020.d(13): Error: declaration expected, not `(`
---
*/
module issue16020;

struct UDA{}
alias Fun = @UDA void();
alias FunTemplate = void(T)(T t);
