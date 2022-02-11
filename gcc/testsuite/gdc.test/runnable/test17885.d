module test17885;

struct T { ulong a, b; }
T f() { return T(); }

void main()
{
    int[T] set = [f(): 0];
    set.remove(f());
    assert(f() !in set);
}
