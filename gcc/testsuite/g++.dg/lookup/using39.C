// { dg-do run }

template <class T>
struct A
{
    int f() { return 1; }
};

template <class T>
struct B : A<T>
{
    int f() { return 2; }

    using A<T>::f;
    void g()
    {
	if (A<T>::f() != 1 )
	    __builtin_abort();

	if( B<T>::f() != 2 )
	    __builtin_abort();

	if( this->f() != 2 )
	    __builtin_abort();
    }
};

template <class T>
struct C
{
    int h( int i ) { return 1; }
    int h( double d ) { return 2; }
};

template <class T>
struct D : private C<T>
{
    using C<T>::h;
    int h( char c ) { return 0; }
    int h() { return 3; }
};

int main()
{
    B<int> b;
    b.g();
    b.f();
    b.A<int>::f();
    b.B<int>::f();

    D<int> d;
    if( d.h( 'a' ) != 0 )
	__builtin_abort();

    if( d.h( 3 ) != 1 )
	__builtin_abort();

    if( d.h( 3.14 ) != 2 )
	__builtin_abort();

    if( d.h() != 3 )
	__builtin_abort();
}
