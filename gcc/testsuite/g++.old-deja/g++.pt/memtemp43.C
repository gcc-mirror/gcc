// Build don't run:
// GROUPS passed templates membertemplates
template<class T, int N>
class A
{
public:
    template<class U>
    void operator=(A<U, N> const & a) { return; }
};

int main()
{
    A<float, 3> a;
    A<double, 3> b;
    a = b;
}
