// PR c++/59978
// { dg-do compile { target c++11 } }

static void testFunc(int i1, int i2) {
    (void)i1;
    (void)i2;
}

template <int... Ints> void wrapper() {
    testFunc(Ints...);
}

int main()
{
    wrapper<1, 2>();
}
