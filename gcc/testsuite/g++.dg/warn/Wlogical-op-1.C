// { dg-do compile}
// { dg-options "-Wlogical-op" }

enum { a, b };

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
    if ( f && b )             // { dg-warning "always evaluate as" }
          do_something(1);
    if ( c && b )             // { dg-warning "always evaluate as" }
          do_something(2);

    if ( b && c == a )        // { dg-bogus "always evaluate as" }
          do_something(101);
    if ( 1 && c )
          do_something(102);  // { dg-bogus "always evaluate as" }
    if ( t2 && b )            // { dg-bogus "always evaluate as" }
          do_something(103);
    if ( true && c == a )     // { dg-bogus "always evaluate as" }
          do_something(104);
    if ( b && true )          // { dg-bogus "always evaluate as" }
          do_something(105);
}
// { dg-do compile}
// { dg-options "-Winvariant-expr" }

enum { a, b };

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
    if ( f && b )             // { dg-warning "always evaluate as" }
          do_something(1);
    if ( c && b )             // { dg-warning "always evaluate as" }
          do_something(2);

    if ( b && c == a )        // { dg-bogus "always evaluate as" }
          do_something(101);
    if ( 1 && c )
          do_something(102);  // { dg-bogus "always evaluate as" }
    if ( t2 && b )            // { dg-bogus "always evaluate as" }
          do_something(103);
    if ( true && c == a )     // { dg-bogus "always evaluate as" }
          do_something(104);
    if ( b && true )          // { dg-bogus "always evaluate as" }
          do_something(105);
}
