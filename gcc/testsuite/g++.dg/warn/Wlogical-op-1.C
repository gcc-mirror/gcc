// { dg-do compile}
// { dg-options "-Wlogical-op" }

enum { a, b1, b2 };

enum testenum { t1, t2};

extern int c;
extern bool bool_a, bool_b;

template<typename Enum>
class QFlags
{
public:
    typedef void **Zero;
    int i;
    inline QFlags(Enum f) : i(f) {}

    inline operator int() const
    { return i;}

};

QFlags<testenum> f(t2);
extern void do_something(int);

extern testenum testa();

void foo()
{
    if ( f && b2 )            // { dg-warning "logical" }
          do_something(1);
    if ( c && b2 )            // { dg-warning "logical" }
          do_something(2);

    if ( b2 && c == a )       // { dg-bogus "logical" }
          do_something(101);
    if ( 1 && c )
          do_something(102);  // { dg-bogus "logical" }
    if ( t2 && b2 )           // { dg-bogus "logical" }
          do_something(103);
    if ( true && c == a )     // { dg-bogus "logical" }
          do_something(104);
    if ( b2 && true )         // { dg-bogus "logical" }
          do_something(105);
}


void bar()
{
    if ( f || b2 )            // { dg-warning "logical" }
          do_something(1);
    if ( c || b2 )            // { dg-warning "logical" }
          do_something(2);

    if ( b2 || c == a )       // { dg-bogus "logical" }
          do_something(101);
    if ( 1 || c )
          do_something(102);  // { dg-bogus "logical" }
    if ( t2 || b2 )           // { dg-bogus "logical" }
          do_something(103);
    if ( true || c == a )     // { dg-bogus "logical" }
          do_something(104);
    if ( b2 || true )         // { dg-bogus "logical" }
          do_something(105);
}
