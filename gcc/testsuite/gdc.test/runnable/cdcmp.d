

bool test_ltz(float x) { return x <  0; }
bool test_lez(float x) { return x <= 0; }
bool test_eqz(float x) { return x == 0; }
bool test_nez(float x) { return x != 0; }
bool test_gez(float x) { return x >= 0; }
bool test_gtz(float x) { return x >  0; }

void test1f()
{
    assert(!test_ltz(0.0f));
    assert(!test_ltz(1.0f));
    assert( test_ltz(-1.0f));

    assert( test_lez(0.0f));
    assert(!test_lez(1.0f));
    assert( test_lez(-1.0f));

    assert( test_eqz(0.0f));
    assert(!test_eqz(1.0f));
    assert(!test_eqz(-1.0f));

    assert(!test_nez(0.0f));
    assert( test_nez(1.0f));
    assert( test_nez(-1.0f));

    assert( test_gez(0.0f));
    assert( test_gez(1.0f));
    assert(!test_gez(-1.0f));

    assert(!test_gtz(0.0f));
    assert( test_gtz(1.0f));
    assert(!test_gtz(-1.0f));
}

bool test_ltz(double x) { return x <  0; }
bool test_lez(double x) { return x <= 0; }
bool test_eqz(double x) { return x == 0; }
bool test_nez(double x) { return x != 0; }
bool test_gez(double x) { return x >= 0; }
bool test_gtz(double x) { return x >  0; }

void test1d()
{
    assert(!test_ltz(0.0));
    assert(!test_ltz(1.0));
    assert( test_ltz(-1.0));

    assert( test_lez(0.0));
    assert(!test_lez(1.0));
    assert( test_lez(-1.0));

    assert( test_eqz(0.0));
    assert(!test_eqz(1.0));
    assert(!test_eqz(-1.0));

    assert(!test_nez(0.0));
    assert( test_nez(1.0));
    assert( test_nez(-1.0));

    assert( test_gez(0.0));
    assert( test_gez(1.0));
    assert(!test_gez(-1.0));

    assert(!test_gtz(0.0));
    assert( test_gtz(1.0));
    assert(!test_gtz(-1.0));
}

bool test_lt(float x, float y) { return x <  y; }
bool test_le(float x, float y) { return x <= y; }
bool test_eq(float x, float y) { return x == y; }
bool test_ne(float x, float y) { return x != y; }
bool test_ge(float x, float y) { return x >= y; }
bool test_gt(float x, float y) { return x >  y; }

void test2f()
{
    assert(!test_lt(1.0f, 1.0f));
    assert( test_lt(1.0f, 2.0f));
    assert(!test_lt(2.0f, 1.0f));

    assert( test_le(1.0f, 1.0f));
    assert( test_le(1.0f, 2.0f));
    assert(! test_le(2.0f, 1.0f));

    assert( test_eq(1.0f, 1.0f));
    assert(!test_eq(1.0f, 2.0f));
    assert(!test_eq(2.0f, 1.0f));

    assert(!test_ne(1.0f, 1.0f));
    assert( test_ne(1.0f, 2.0f));
    assert( test_ne(2.0f, 1.0f));

    assert( test_ge(1.0f, 1.0f));
    assert(!test_ge(1.0f, 2.0f));
    assert( test_ge(2.0f, 1.0f));

    assert(!test_gt(1.0f, 1.0f));
    assert(!test_gt(1.0f, 2.0f));
    assert( test_gt(2.0f, 1.0f));
}

bool test_lt(double x, double y) { return x <  y; }
bool test_le(double x, double y) { return x <= y; }
bool test_eq(double x, double y) { return x == y; }
bool test_ne(double x, double y) { return x != y; }
bool test_ge(double x, double y) { return x >= y; }
bool test_gt(double x, double y) { return x >  y; }

void test2d()
{
    assert(!test_lt(1.0, 1.0));
    assert( test_lt(1.0, 2.0));
    assert(!test_lt(2.0, 1.0));

    assert( test_le(1.0, 1.0));
    assert( test_le(1.0, 2.0));
    assert(! test_le(2.0, 1.0));

    assert( test_eq(1.0, 1.0));
    assert(!test_eq(1.0, 2.0));
    assert(!test_eq(2.0, 1.0));

    assert(!test_ne(1.0, 1.0));
    assert( test_ne(1.0, 2.0));
    assert( test_ne(2.0, 1.0));

    assert( test_ge(1.0, 1.0));
    assert(!test_ge(1.0, 2.0));
    assert( test_ge(2.0, 1.0));

    assert(!test_gt(1.0, 1.0));
    assert(!test_gt(1.0, 2.0));
    assert( test_gt(2.0, 1.0));
}

int main()
{
    test1f();
    test1d();
    test2f();
    test2d();
    return 0;
}
