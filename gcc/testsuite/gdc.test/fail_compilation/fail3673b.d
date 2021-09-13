/*
TEST_OUTPUT:
---
fail_compilation/fail3673b.d(12): Error: basic type expected, not `if`
fail_compilation/fail3673b.d(12): Error: template constraints only allowed for templates
fail_compilation/fail3673b.d(12): Error: { } expected following `class` declaration
fail_compilation/fail3673b.d(12): Error: no identifier for declarator `A`
fail_compilation/fail3673b.d(12): Error: declaration expected, not `{`
---
*/
class A {}
class B : if(false) A { }
