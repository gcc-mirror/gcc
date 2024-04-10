// PR c++/107126
// { dg-options "" }

template<typename T>
struct C
{
    ~C();
};
template<typename T>
C<T>::~C<T>()	      // { dg-warning "template-id not allowed for destructor" "" { target c++20 } }
{
}
int main()
{
    C<int> c;;
}
