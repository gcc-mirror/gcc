// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/imports/a15667.d(16): Error: basic type expected, not `;`
fail_compilation/imports/a15667.d(19): Error: declaration expected following attribute, not end of file
---
*/

void main()
{
    import imports.a15667;
}
