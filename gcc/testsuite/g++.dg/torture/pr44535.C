/* { dg-do run } */

namespace FOO {

template <typename T>
class A
{
public:
    void Enum();
    virtual void OnProv() = 0;
    virtual ~A() { }
};
typedef A<char> B;

template<typename T>
void A<T>::Enum ()
{
    OnProv ();
}
} // namespace FOO

class C {};

class D: public C, public FOO::B {
public:
    void OnProv() {}
};

int main(int argc, char *argv[])
{
    D x;
    x.Enum();
    return 0;
}
