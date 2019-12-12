/*
TEST_OUTPUT:
---
fail_compilation/parse12924.d(14): Error: declaration expected following attribute, not `;`
fail_compilation/parse12924.d(15): Error: declaration expected following attribute, not `;`
fail_compilation/parse12924.d(16): Error: declaration expected following attribute, not `;`
fail_compilation/parse12924.d(17): Error: declaration expected following attribute, not `;`
fail_compilation/parse12924.d(18): Error: declaration expected following attribute, not `;`
fail_compilation/parse12924.d(19): Error: declaration expected following attribute, not `;`
fail_compilation/parse12924.d(20): Error: declaration expected following attribute, not `;`
---
*/

static;         void f1() {}
deprecated;     void f2() {}
deprecated(""); void f3() {}
extern(C);      void f4() {}
public;         void f5() {}
align(1);       void f6() {}
@(1);           void f7() {}
