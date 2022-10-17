// EXTRA_FILES: test18951a.d
module compilable.test18951b;

import test18951a;

void test()
{
    A.foo(new Object);
}
