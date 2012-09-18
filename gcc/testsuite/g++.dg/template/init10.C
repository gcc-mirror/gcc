template <int N>
struct A { };

template <int Q>
void g()
{
    const int M ( Q );

    A<M> a;
}

void h()
{
    g<3>();
}
