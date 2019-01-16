/* TEST_OUTPUT:
---
fail_compilation/cwords.d(13): Error: undefined identifier `FALSE`, did you mean `false`?
fail_compilation/cwords.d(14): Error: undefined identifier `TRUE`, did you mean `true`?
fail_compilation/cwords.d(15): Error: undefined identifier `NULL`, did you mean `null`?
fail_compilation/cwords.d(16): Error: undefined identifier `unsigned`, did you mean `uint`?
---
*/


void foo()
{
    bool a = FALSE;
    bool b = TRUE;
    int* p = NULL;
    unsigned u;
}
