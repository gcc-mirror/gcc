module dmd.compiler.test.fail_compilation.test21317;

/* TEST_OUTPUT:
---
fail_compilation/test21317.d(16): Error: `writeln` is not defined, perhaps `import std.stdio;` ?
fail_compilation/test21317.d(21): Error: undefined identifier `Zero`, did you mean variable `zero`?
fail_compilation/test21317.d(25): Error: undefined identifier `NULL`, did you mean `null`?
fail_compilation/test21317.d(31): Error: undefined identifier `this`, did you mean `typeof(this)`?
fail_compilation/test21317.d(38): Error: undefined identifier `unknown`
---
*/

// Weird formatting is intended to check the loc

int[
  writeln
  ] arr1;

int zero = 0;
int [
  Zero
  ] arr2;

int [
  NULL
  ] arr3;

struct S4
{
    enum E4 :
    this
    {
        fail,
    }
}

int [
  unknown
  ] arr5;
