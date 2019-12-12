// Bugzilla 8150: nothrow check doesn't work for constructor

struct Foo
{
    this(int) nothrow
    {
        throw new Exception("something");
    }
}

void main() {
    Foo(1);
}
