extern(C) int printf(const char*, ...);

alias TypeTuple(T...) = T;

/***************************************************/
// 13336

struct S13336
{
    int opApply(scope int delegate(int) dg)
    {
        return dg(0);
    }
}

double result13336;

enum fbody13336 =
q{
    static if (n == 1)
    {
        if (f)
            return sx;
        return sy;
    }
    static if (n == 2)
    {
        foreach (e; S13336())
        {
            if (f)
                return sx;
            return sy;
        }
        assert(0);
    }
    static if (n == 3)
    {
        if (f)
            return sx;
        foreach (e; S13336())
        {
            return sy;
        }
        assert(0);
    }
    static if (n == 4)
    {
        foreach (e; S13336())
        {
            if (f)
                return sx;
        }
        return sy;
    }
    static if (n == 5)
    {
        if (false)
            return 99;
        foreach (e; S13336())
        {
            if (f)
                return sx;
            return sy;
        }
        assert(0);
    }
    static if (n == 6)
    {
        foreach (e; S13336())
        {
            if (f)
                return sx;
            return sy;
        }
        return 99;
    }
};

// auto ref without out contract
auto ref f13336a(int n, alias sx, alias sy)(bool f)
{
    mixin(fbody13336);
}

// auto without out contract
auto f13336b(int n, alias sx, alias sy)(bool f)
{
    mixin(fbody13336);
}

// auto ref with out contract
auto ref f13336c(int n, alias sx, alias sy)(bool f)
out(r)
{
    static assert(is(typeof(r) == const double));
    assert(r == (f ? sx : sy));
    result13336 = r;
}
body
{
    mixin(fbody13336);
}

// auto with out contract
auto f13336d(int n, alias sx, alias sy)(bool f)
out(r)
{
    static assert(is(typeof(r) == const double));
    assert(r == (f ? sx : sy));
    result13336 = r;
}
body
{
    mixin(fbody13336);
}

void test13336()
{
    static int sx = 1;
    static double sy = 2.5;

    foreach (num; TypeTuple!(1, 2, 3, 4, 5, 6))
    {
        foreach (foo; TypeTuple!(f13336a, f13336b))
        {
            alias fooxy = foo!(num, sx, sy);
            static assert(is(typeof(&fooxy) : double function(bool)));
            assert(fooxy(1) == 1.0);
            assert(fooxy(0) == 2.5);

            alias fooyx = foo!(num, sy, sx);
            static assert(is(typeof(&fooyx) : double function(bool)));
            assert(fooyx(1) == 2.5);
            assert(fooyx(0) == 1.0);
        }

        foreach (foo; TypeTuple!(f13336c, f13336d))
        {
            alias fooxy = foo!(num, sx, sy);
            static assert(is(typeof(&fooxy) : double function(bool)));
            assert(fooxy(1) == 1.0 && result13336 == 1.0);
            assert(fooxy(0) == 2.5 && result13336 == 2.5);

            alias fooyx = foo!(num, sy, sx);
            static assert(is(typeof(&fooyx) : double function(bool)));
            assert(fooyx(1) == 2.5 && result13336 == 2.5);
            assert(fooyx(0) == 1.0 && result13336 == 1.0);
        }
    }
}

/***************************************************/
// 15018

struct S15018(int n)
{
    short[n] m;
}

S15018!n f15018(int n)()
{
    S15018!n s;
    foreach(i; 0..n)
        s.m[i] = cast(short)(i * i + 3);
    return s;
}

void test15018()
{
    // size 4
    S15018!2[3] s3;
    s3[] = f15018!2();
    foreach (int i; 0..3)
    {
        assert(s3[i].m[0] == 3);
        assert(s3[i].m[1] == 4);
    }

    // size 4-18
    foreach (n; TypeTuple!(2, 3, 4, 5, 6, 7, 8, 9))
    {
        S15018!n[5] i5;
        i5[] = f15018!n();
        foreach (int j; 0..5)
            foreach(k; 0..n)
                if (i5[j].m[k] != k * k + 3)
                    assert(false);
    }
}

/***************************************************/

int main()
{
    test13336();
    test15018();

    printf("Success\n");
    return 0;
}
