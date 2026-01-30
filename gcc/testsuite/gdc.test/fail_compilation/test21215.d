/* TEST_OUTPUT:
---
fail_compilation/test21215.d(13): Error: `y` is not a member of `S`
fail_compilation/test21215.d(18): Error: `xhs` is not a member of `S`, did you mean variable `xsh`?
fail_compilation/test21215.d(28): Error: `y` is not a member of `S`
fail_compilation/test21215.d(32): Error: `yashu` is not a member of `S`
---
*/
struct S { int xsh; }

void test() {
	auto s1 = S(
		y:
    1
	);

  auto s2 = S(
    xhs:
    1
  );

  auto s3 = S(
    xsh: 1
  );

  auto s4 = S(
    xsh: 1,
    y: 2
  );

  auto s5 = S(
    yashu:
    2
  );
}
