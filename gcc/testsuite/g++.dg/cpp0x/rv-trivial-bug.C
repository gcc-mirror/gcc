// { dg-do "run" }
// { dg-options "-std=c++0x" }
// PR c++/33235
#include <cassert>

int move_construct = 0;
int move_assign = 0;

struct base2
{
    base2() {}
    base2(base2&&) {++move_construct;}
    base2& operator=(base2&&) {++move_assign; return *this;}
};

int test2()
{
    base2 b;
    base2 b2(b);
    assert(move_construct == 0);
    base2 b3(static_cast<base2&&>(b));
    assert(move_construct == 1);
    b = b2;
    assert(move_assign == 0);
    b = static_cast<base2&&>(b2);
    assert(move_assign == 1);
}

int main()
{
    test2();
    return 0;
}
