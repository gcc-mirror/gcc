// DFLAGS:
// REQUIRED_ARGS: -defaultlib=
// EXTRA_SOURCES: extra-files/minimal/object.d

// This test ensures that interfaces and classes can be used in a minimal
// runtime as long as they only contain static members.

// This should compile, but will not link and run properly without
// a thread-local storage (TLS) implementation.

interface I
{
    static int i;
}

class A : I
{
    static int a;
}

class B : A
{
    static int b;
}

void main()
{
    B.i = 32;
    B.a = 42;
    B.b = 52;
}
