// Build don't run:
// GROUPS passed templates membertemplates
template<class T>
class A
{
};

template<>
class A<float>
{
public:
    template<class U>
    void func(U v1 = 0) {}
};

int main()
{
  A<float> a;
  a.func(3);
}
