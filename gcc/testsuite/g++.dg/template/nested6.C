// PR c++/63309

template <class T>
class A
{
public:
    class B;
};

template <class T, class I>
class A<T>::B			// { dg-error "template parameters|required" }
{
};

int main()
{
    A<int>::B myB;		// { dg-prune-output "incomplete type" }
    return 0;
}
