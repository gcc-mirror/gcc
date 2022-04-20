/*
TEST_OUTPUT:
---
fail_compilation/fail258.d(101): Error: delimiter cannot be whitespace
fail_compilation/fail258.d(101): Error: delimited string must end in `"`
fail_compilation/fail258.d(101): Error: declaration expected, not `"X"`
fail_compilation/fail258.d(104): Error: unterminated string constant starting at fail_compilation/fail258.d(104)
---
*/

#line 100

q"
X

X"
