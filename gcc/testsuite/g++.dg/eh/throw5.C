// PR c++/57533
// { dg-do run { target c++11 } }

struct X
{
    bool moved = false;

    X() = default;
    X(const X&) = default;
    X(X&& x) { x.moved = true; }
};

int main()
{
    X x;
    try {
        throw x;
    }
    catch(...) {
    }
    if (x.moved)
        __builtin_abort();
}
