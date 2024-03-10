/*
TEST_OUTPUT:
---
fail_compilation/unmatchedbrace.d(11): Error: matching `}` expected, not `End of File`
fail_compilation/unmatchedbrace.d(8):        unmatched `{`
---
*/
@safe {
    //
    struct S {}
