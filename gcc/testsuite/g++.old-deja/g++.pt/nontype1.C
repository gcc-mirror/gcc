// Build don't link:

template<int N_length>
struct B
{
    B();
    ~B();
};
template<class P, int N>
struct D
{
    D(int r0);
    D(B<N-1> &, int);
};
template<class T>
void func()
{
    D<T,1> tmp;
}
