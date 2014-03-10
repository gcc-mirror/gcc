// PR c++/46552

struct S
{
    int x;
};

template < typename >
void f( void )
{
    &S::x;
}
