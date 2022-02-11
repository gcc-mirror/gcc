/* TEST_OUTPUT:
---
fail_compilation/test20696.d(106): Error: function `test20696.S!().S.test` cannot retrieve its `.mangleof` while inferring attributes
fail_compilation/test20696.d(106):        while evaluating `pragma(msg, test.mangleof)`
fail_compilation/test20696.d(111): Error: template instance `test20696.S!()` error instantiating
---
*/

#line 100

// https://issues.dlang.org/show_bug.cgi?id=20696

struct S()
{
  int test() {
    pragma(msg, test.mangleof);
    return 3;
  }
}

S!() s;
