void test7603()
{
    int g;
    void foo(int n, ref int r = g) { r = n; }

    int x;
    foo(1, x);
    assert(x == 1);

    foo(2);
    assert(g == 2);

    int h = 100;
    void bar(int n, out int r = h) { if (n != 0) r = n;  }

    bar(0);
    assert(h == 0);

    bar(10);
    assert(h == 10);

    bar(10, x);
    assert(x == 10);

    bar(0, x);
    assert(x == 0);

}

void main() { test7603(); }
