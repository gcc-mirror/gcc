// PR c++/39225

template <class T>
class A
{
public:
    A() {}
    ~B() {}			// { dg-error "~B" }
};

int main()
{
    A<int> *a = new A<int>;

    return 0;
}
