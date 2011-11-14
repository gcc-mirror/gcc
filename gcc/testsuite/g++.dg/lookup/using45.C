// PR c++/30195
// { dg-do run }

template <class T>
struct A
{
    int f(int) { return 0; }
    int f(double) { return 1; }
    int f(char) { return 2; }
};

template <class T>
struct B : A<T>
{
    using A<T>::f;
    int f(int) { return 3; }
};

int main()
{
    B<int> b;
    if( b.f( 42 ) != 3 )
	__builtin_abort();

    if( b.f( 3.14 ) != 1 )
	__builtin_abort();

    if( b.f( 'a' ) != 2 )
	__builtin_abort();

    if( b.A<int>::f( 42 ) != 0 )
	__builtin_abort();
}
