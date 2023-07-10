/*
TEST_OUTPUT:
---
fail_compilation/missingbrace.d(11): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/missingbrace.d(9):        unmatched `{`
---
*/
void main()
{
    int a;
