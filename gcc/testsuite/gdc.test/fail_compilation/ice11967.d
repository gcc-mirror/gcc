/*
TEST_OUTPUT:
---
fail_compilation/ice11967.d(12): Error: use `@(attributes)` instead of `[attributes]`
fail_compilation/ice11967.d(12): Error: expression expected, not `%`
fail_compilation/ice11967.d(12): Error: found `g` when expecting `)`
fail_compilation/ice11967.d(12): Error: found `{` when expecting `]`
fail_compilation/ice11967.d(13): Error: `@identifier` or `@(ArgumentList)` expected, not `@End of File`
fail_compilation/ice11967.d(13): Error: declaration expected following attribute, not end of file
---
*/
[F(%g{@
