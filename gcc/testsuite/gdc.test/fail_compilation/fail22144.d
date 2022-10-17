// https://issues.dlang.org/show_bug.cgi?id=22144
/* TEST_OUTPUT:
---
fail_compilation/fail22144.d(12): Error: cannot cast expression `zarray1` of type `int[0]` to `int[0][]` since sizes don't line up
---
*/
void main()
{
  int[0] zarray1;
  int[0][0] zarray2;

  auto zslice1 = cast(int[0][])zarray1; // ICE -> Error
  auto zslice2 = cast(int[0][])zarray2; // ICE -> OK
}
