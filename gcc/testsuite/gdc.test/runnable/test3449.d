
/******************************************/
// 3449

template TypeTuple(T...) { alias TypeTuple = T; }

// If module variable has no explicit initializer,
// constant folding is not allowed for that.
int mg1;
const int cg1;
immutable int ig1;
static this()
{
    mg1 = 10;
    cg1 = 10;
    ig1 = 10;
}
static assert(!__traits(compiles, { static assert(mg1 == 0); }));
static assert(!__traits(compiles, { static assert(cg1 == 0); }));
static assert(!__traits(compiles, { static assert(ig1 == 0); }));

// But, if module variable has explicit initializer and
// non-mutable type, constant folding is allowed for that..
int mg2 = 1;
const int cg2 = 1;
immutable int ig2 = 1;
static this()
{
    mg2 = 11;
    static assert(!__traits(compiles, cg2 = 11));   // not allowed for constant folding
    static assert(!__traits(compiles, ig2 = 11));   // not allowed for constant folding
}
static assert(!__traits(compiles, { static assert(mg2 == 1); }));
                                    static assert(cg2 == 1);    // possible
                                    static assert(ig2 == 1);    // possible

// For aggregate fields, compiler behavior will be changed.
void test3449()
{
    static struct S(T)
    {
        T field1;       // doesn't have explicit initializer
        T field2 = 1;   // has explicit initializer

        this(int n)
        {
            field1 = n; // allowed
            field2 = n; // NEW! re-assigning during construction is allowed for any qualified fields.
        }
    }

    foreach (T; TypeTuple!(int, const int, immutable int))
    {
        alias S!T ST;

        auto s1 = ST();             // default construction
        assert(s1.field1 == 0);     // == T.init
        assert(s1.field2 == 1);     // == specified initializer

        // Getting address for non-mutable field is allowed.
        T* s1p1 = &s1.field1;
        T* s1p2 = &s1.field2;
        assert(*s1p1 == 0);
        assert(*s1p2 == 1);
        static if (is(T == int))
        {   // If T is mutable,
            // modification through indirection is allowed
            *s1p1 = 100, *s1p2 = 101;
            assert(*s1p1 == 100);
            assert(*s1p2 == 101);
        }
        else
        {   // If T is not mutable, modification is not allowed
            static assert(!__traits(compiles, *s1p1 = 100));
            static assert(!__traits(compiles, *s1p2 = 100));
        }

        // Access to non-static non-mutable field is
        // now correctly rejected by "need this" error.
        static assert(!__traits(compiles, ST.field1 == 1));
        static assert(!__traits(compiles, ST.field2 == 0));

        // So, re-assignment of non-mutable fields
        // during construction is enough acceptable.
        auto s2 = ST(10);
        assert(s2.field1 == 10);
        assert(s2.field2 == 10);
    }
}

/******************************************/
// 10643

struct S10643
{
    const int[1000] x = void;

    this(int n)
    {
        x[] = n;
    }
}
static assert(S10643.sizeof == int.sizeof * 1000);

/******************************************/

int main()
{
    test3449();

    return 0;
}
