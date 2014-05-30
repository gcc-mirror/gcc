// PR c++/56947

struct A
{
    A (int);
};

template < typename >
void Fn ()
{
    const int kCapacity = 0;
    struct Q:A
    {
        Q ():A (kCapacity) { }
    };
    Q q;
}
template void Fn < int >();
