/*
TEST_OUTPUT:
---
fail_compilation/fail258.d(11): Error: delimiter cannot be whitespace
fail_compilation/fail258.d(11): Error: delimited string must end in 
"
fail_compilation/fail258.d(11): Error: declaration expected, not `"X"`
fail_compilation/fail258.d(14): Error: unterminated string constant starting at fail_compilation/fail258.d(14)
---
*/
q"
X

X"

