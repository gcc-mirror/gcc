// PR c++/36797

template <int> struct A { };

template <class T>
int foo (A<__is_empty (T)>* = 0); // { dg-error "built-in trait" }

int main ()
{
    foo<int>();
}
