struct A
{
    int foo () const { return 0; }
};

template <typename> void bar (int x[], const A &a)
{
    const int i=a.foo();
    x[i]=0;
}
