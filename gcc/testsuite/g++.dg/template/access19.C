/* PR c++/29475 The error diagnostic contained "U = U" instead of "U = char" */
/* { dg-do "compile" } */

template< class T >
class explicit_t
{
public:
        explicit_t( const T& c ): value( c ) { }
        operator T&() { return value; }
private:
        template< class U >
        explicit_t( U t ); /* { dg-error "with U = char, T = int|is private" } */
        T value;
};

int foo( int x, explicit_t< int > y )
{
        return x + y;
}

int main()
{
        return foo( 5, 'c' ); /* { dg-error "this context" } */
}
