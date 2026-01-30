// https://github.com/dlang/dmd/issues/17487
import core.stdc.stdio;

// version = PRINT;

void testKey()
{
    version(PRINT) printf("--- testKey\n");
    auto count = new Count;

    string[S] aa;
    aa[S(count, 42)] = "foo";

    version(PRINT) printf("--- after init\n");
    version(PRINT) count.print();
    assert(count.refCount == 1);

    version(PRINT) printf("---\n");
    assert(S(count, 42) in aa);

    version(PRINT) printf("--- after in\n");
    version(PRINT) count.print();
    assert(count.refCount == 1);

    version(PRINT) printf("---\n");
    assert(S(new Count, 22) !in aa);
    version(PRINT) printf("--- after !in\n");
    version(PRINT) count.print();
    assert(count.refCount == 1);

    version(PRINT) printf("---\n");
    aa[S(count, 42)] = "foo2";

    version(PRINT) printf("--- after assign value\n");
    version(PRINT) count.print();
    assert(count.refCount == 1);
    assert(count.copyConstructed == 1); // copy ctor used when adding key, not during lookup
}

void testValue()
{
    version(PRINT) printf("--- testValue\n");
    auto count = new Count;

    S[int] aa;
    aa[42] = S(count, 42);

    version(PRINT) printf("--- after init\n");
    version(PRINT) count.print();
    assert(count.refCount == 1);
    assert(count.constructed == 1);

    version(PRINT) printf("---\n");
    aa[22] = S(count, 22);

    version(PRINT) printf("--- after create by value\n");
    version(PRINT) count.print();
    assert(count.refCount == 2);
    assert(count.constructed == 2);

    version(PRINT) printf("---\n");
    aa[2] = aa[22];

    version(PRINT) printf("--- after create by copy ctor\n");
    version(PRINT) count.print();
    assert(count.refCount == 3);
    assert(count.copyConstructed == 1);

    version(PRINT) printf("---\n");
    aa[22] = aa[42];

    version(PRINT) printf("--- after assign by copy\n");
    version(PRINT) count.print();
    assert(count.refCount == 3);
    assert(count.assignedFrom == 1);
    assert(count.assignedTo == 1);

    version(PRINT) printf("---\n");
}

void main()
{
    testKey();
    testValue();
    version(PRINT) printf("--- cleanup\n");
}

struct Count
{
    int constructed;
    int copyConstructed;
    int assignedFrom;
    int assignedTo;
    int destroyed;
    int refCount;

    void print()
    {
        printf("Constructed: %d times\n", constructed);
        printf("Copy Constructed: %d times\n", copyConstructed);
        printf("Assigned from: %d times\n", assignedFrom);
        printf("Assigned To: %d times\n", assignedTo);
        printf("Destroyed: %d times\n", destroyed);
        printf("refCount: %d\n", refCount);
    }
}

struct S
{
    Count* count;
    int i;
    bool destroyed;

    this(Count* count, int i)
    {
        this.count = count;
        this.i = i;
        ++count.constructed;
        ++count.refCount;
        version(PRINT) printf("Constructing: %d, count(%p)\n", i, count);
    }

    this(ref S rhs)
    {
        this.count = rhs.count;
        this.i = rhs.i;

        ++count.copyConstructed;
        ++count.refCount;
        version(PRINT) printf("Copy Constructing: %d, count(%p)\n", i, count);
    }

    ~this()
    {
        version(PRINT) printf("Destroying: %d, count(%p)\n", i, count);

        if(count)
        {
            ++count.destroyed;
            --count.refCount;
        }

        destroyed = true;
    }

    void opAssign()(auto ref S rhs)
    {
        version(PRINT) printf("Assigning %d, count(%p) to %d, count(%p)\n",
            rhs.i, rhs.count, this.i, this.count);

        if(this.count)
        {
            ++this.count.assignedTo;
            --this.count.refCount;
        }

        if(rhs.count)
        {
            ++rhs.count.assignedFrom;
            ++rhs.count.refCount;
        }

        this.count = rhs.count;
        this.i = rhs.i;
    }

    invariant
    {
        assert(!destroyed);
    }

    bool opEquals()(const auto ref S rhs) const {
        return this.i == rhs.i;
    }

    size_t toHash() const {
        return 0;
    }
}
