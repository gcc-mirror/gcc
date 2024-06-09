/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/goto_skip.d(28): Error: `goto` skips declaration of variable `goto_skip.skip.ch`
        goto Lskip;
        ^
fail_compilation/goto_skip.d(29):        declared here
    char ch = '!';
         ^
fail_compilation/goto_skip.d(36): Error: `goto` skips declaration of `with` temporary
    goto L1;
    ^
fail_compilation/goto_skip.d(38):        declared here
    with (S()) {
    ^
fail_compilation/goto_skip.d(46): Error: `goto` skips declaration of variable `goto_skip.test8.e`
  goto L2;
  ^
fail_compilation/goto_skip.d(51):        declared here
  catch (Exception e) {
  ^
---
*/
char skip(bool b)
{
    if (b)
        goto Lskip;
    char ch = '!';
Lskip:
    return ch;
}

int f()
{
    goto L1;
    struct S { int e = 5; }
    with (S()) {
L1:
        return e;
    }
}

void test8(int a)
{
  goto L2;

  try {
      a += 2;
  }
  catch (Exception e) {
      a += 3;
L2: ;
      a += 100;
  }
  assert(a == 100);
}
