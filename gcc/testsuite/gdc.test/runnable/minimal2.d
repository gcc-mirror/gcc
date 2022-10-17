// DFLAGS:
// REQUIRED_ARGS: -defaultlib=
// EXTRA_SOURCES: extra-files/minimal/object.d

// This test ensures that interfaces and classes can be used in a minimal
// runtime as long as they only contain shared static members.  Non-shared
// static members would require a thread-local storage (TLS) implementation.

interface I
{
    shared static int i;
}

class A : I
{
    shared static int a;
}

class B : A
{
    shared static int b;

    static int sumAll()
    {
        return b + a + i;
    }
}

void poorMansAssert(bool condition)
{
    if (!condition)
    {
        static char* hlt;
        *hlt = 0;
    }
}

void main()
{
    B.i = 32;
    B.a = 42;
    B.b = 52;

    poorMansAssert(B.i == 32 || B.a == 42 || B.b == 52);
    poorMansAssert(B.sumAll() == (32 + 42 + 52));
}
