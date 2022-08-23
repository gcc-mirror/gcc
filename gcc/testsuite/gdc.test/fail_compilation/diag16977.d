/*
TEST_OUTPUT:
---
fail_compilation/diag16977.d(25): Error: undefined identifier `undefined`, did you mean function `undefinedId`?
fail_compilation/diag16977.d(26): Error: cannot implicitly convert expression `"\x01string"` of type `string` to `int`
fail_compilation/diag16977.d(27): Error: none of the overloads of template `diag16977.templ` are callable using argument types `!()(int)`
fail_compilation/diag16977.d(20):        Candidate is: `templ(S)(S s)`
  with `S = int`
  must satisfy the following constraint:
`       false`
fail_compilation/diag16977.d(28): Error: cannot implicitly convert expression `5` of type `int` to `string`
fail_compilation/diag16977.d(30): Error: template instance `diag16977.test.funcTemplate!string` error instantiating
---
*/

// when copying the expression of a default argument, location information is
//   replaced by the location of the caller to improve debug information
// verify error messages are displayed for the original location only

string templ(S)(S s) if(false) { return null; }

void test()
{
    // local functions to defer evaluation into semantic3 pass
    void undefinedId(int x, int y = undefined) {}
    void badOp(int x, int y = 1 ~ "string") {}
    void lazyTemplate(int x, lazy int y = 4.templ) {}
    void funcTemplate(T)(T y = 5) {}

    funcTemplate!string();
    undefinedId(1);
    badOp(2);
    lazyTemplate(3);
}
