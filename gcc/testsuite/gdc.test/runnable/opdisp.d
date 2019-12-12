extern (C) int printf(const char* fmt, ...);

int pass(int n){ return n; }

struct X
{
    int m;

    int opIndex(int m, int n)
    {
        return n;
    }
}

/**********************************************/

struct S1f
{
    int opDispatch(string name, A...)(A args)
    {
      static if (args.length)
        return args[0];
      else
        return 0;
    }
}
struct S1p
{
    @property int opDispatch(string name, A...)(A args)
    {
      static if (args.length)
        return args[0];
      else
        return 0;
    }
}
void test1()
{
    S1f s1f;
    assert(s1f.func() == 0);            // ok -> ok
    assert(s1f.func(1) == 1);           // ok -> ok
    assert(pass(s1f.func()) == 0);      // ok -> ok
    assert(pass(s1f.func(1)) == 1);     // ok -> ok
    assert(X(s1f.func()).m == 0);
    assert(X()[0, s1f.func()] == 0);

    S1p s1p;
    assert(s1p.prop == 0);              // ok   -> ok
    assert((s1p.prop = 1) == 1);        // CTng -> CTng
    assert(pass(s1p.prop) == 0);        // ok   -> ok
    assert(pass(s1p.prop = 2) == 2);    // CTng -> CTng
    assert(X(s1p.prop).m == 0);
    assert(X()[0, s1p.prop] == 0);
}

/**********************************************/

struct S2f
{
    template opDispatch(string name)
    {
        int opDispatch(A...)(A args)
        {
          static if (args.length)
            return args[0];
          else
            return 0;
        }
    }
}
struct S2p
{
    template opDispatch(string name)
    {
        @property int opDispatch(A...)(A args)
        {
          static if (args.length)
            return args[0];
          else
            return 0;
        }
    }
}
void test2()
{
    S2f s2f;
    assert(s2f.func() == 0);            // ok -> ok
    assert(s2f.func(1) == 1);           // ok -> ok
    assert(pass(s2f.func()) == 0);      // ok -> ok
    assert(pass(s2f.func(1)) == 1);     // ok -> ok
    assert(X(s2f.func()).m == 0);
    assert(X()[0, s2f.func()] == 0);

    S2p s2p;
    assert(s2p.prop == 0);              // CTng -> ok
    assert((s2p.prop = 1) == 1);        // ok   -> ok
    assert(pass(s2p.prop) == 0);        // CTng -> ok
    assert(pass(s2p.prop = 2) == 2);    // ok   -> ok
    assert(X(s2p.prop).m == 0);
    assert(X()[0, s2p.prop] == 0);
}

/**********************************************/

struct S3f
{
    template opDispatch(string name)
    {
        template opDispatch(T)
        {
            int opDispatch(A...)(A args)
            {
              static if (args.length)
                return args[0];
              else
                return 0;
            }
        }
    }
}
struct S3p
{
    template opDispatch(string name)
    {
        template opDispatch(T)
        {
            @property int opDispatch(A...)(A args)
            {
              static if (args.length)
                return args[0];
              else
                return 0;
            }
        }
    }
}
void test3()
{
    S3f s3f;
    assert(s3f.func!int() == 0);            // ok -> ok
    assert(s3f.func!int(1) == 1);           // ok -> ok
    assert(pass(s3f.func!int()) == 0);      // ok -> ok
    assert(pass(s3f.func!int(1)) == 1);     // ok -> ok
    assert(X(s3f.func!int()).m == 0);
    assert(X()[0, s3f.func!int()] == 0);

    S3p s3p;
    assert(s3p.prop!int == 0);              // CTng -> ok
    assert((s3p.prop!int = 1) == 1);        // ok   -> ok
    assert(pass(s3p.prop!int) == 0);        // CTng -> ok
    assert(pass(s3p.prop!int = 2) == 2);    // ok   -> ok
    assert(X(s3p.prop!int).m == 0);
    assert(X()[0, s3p.prop!int] == 0);
}

/**********************************************/

struct S4f
{
    ref int opDispatch(string name, A...)(A args)
    {
        static int n;
        n = args.length;
        return n;
    }
}
struct S4p
{
    @property ref int opDispatch(string name, A...)(A args)
    {
        static int n;
        n = args.length;
        return n;
    }
}
void test4()
{
    S4f s4f;
    assert(s4f.func == 0);          // getter
    assert((s4f.func = 1) == 1);    // setter

    S4p s4p;
    assert(s4p.prop == 0);          // getter
    assert((s4p.prop = 1) == 1);    // setter
}

/**********************************************/

struct S5f
{
    template opDispatch(string name)
    {
        ref int opDispatch(A...)(A args)
        {
            static int n;
            n = args.length;
            return n;
        }
    }
}
struct S5p
{
    template opDispatch(string name)
    {
        @property ref int opDispatch(A...)(A args)
        {
            static int n;
            n = args.length;
            return n;
        }
    }
}
void test5()
{
    S5f s5f;
    assert(s5f.prop == 0);          // getter   ng -> ok
    assert((s5f.prop = 1) == 1);    // setter

    S5p s5p;
    assert(s5p.prop == 0);          // getter   ng -> ok
    assert((s5p.prop = 1) == 1);    // setter
}

/**********************************************/

struct S6f
{
    template opDispatch(string name)
    {
        template opDispatch(T)
        {
            ref int opDispatch(A...)(A args)
            {
                static int n;
                n = args.length;
                return n;
            }
        }
    }
}
struct S6p
{
    template opDispatch(string name)
    {
        template opDispatch(T)
        {
            @property ref int opDispatch(A...)(A args)
            {
                static int n;
                n = args.length;
                return n;
            }
        }
    }
}
void test6()
{
    S6f s6f;
    assert(s6f.prop!int == 0);          // getter   ng -> ok
    assert((s6f.prop!int = 1) == 1);    // setter

    S6p s6p;
    assert(s6p.prop!int == 0);          // getter   ng -> ok
    assert((s6p.prop!int = 1) == 1);    // setter
}

/**********************************************/
// 7578

struct Foo7578
{
    static int[] opDispatch(string op, Args...)(Args)
    {
        return [0];
    }
}

void test7578()
{
    Foo7578.attrs[0] = 1;
}

/**********************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7578();

    printf("Success\n");
    return 0;
}
