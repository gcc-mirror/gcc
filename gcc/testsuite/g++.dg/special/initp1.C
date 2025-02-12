/* { dg-do run { target init_priority } } */
#include <stdlib.h>

class Two {
private:
    int i, j, k;
public:
    static int count;
    Two( int ii, int jj ) { i = ii; j = jj; k = count++; };
    Two( void )           { i =  0; j =  0; k = count++; };
    int eye( void ) { return i; };
    int jay( void ) { return j; };
    int kay( void ) { return k; };
};

extern Two foo;
extern Two goo;
extern Two coo[];
extern Two koo[];

Two foo __attribute__((init_priority(1005))) ( 5, 6 );

Two goo __attribute__((init_priority(1007))) = Two( 7, 8 );

Two doo[ 3 ];

Two hoo[ 3 ] = {
    Two( 11, 12 ),
    Two( 13, 14 ),
    Two( 15, 16 )
};

Two coo[ 3 ] __attribute__((init_priority(10))); // { dg-warning "reserved" }
#pragma GCC diagnostic ignored "-Wprio-ctor-dtor"
Two koo[ 3 ] __attribute__((init_priority(10))) = {
    Two( 21, 22 ),
    Two( 23, 24 ),
    Two( 25, 26 )
};

Two xoo[ 3 ] __attribute__((init_priority(1100)));

Two zoo[ 3 ] __attribute__((init_priority(1100))) = {
    Two( 31, 32 ),
    Two( 33, 34 ),
    Two( 35, 36 )
};

int Two::count;

long x = 0;

#define X( n ) \
  do { if ( x & (1L << (n)) ) return 1; else x |= (1L << (n)); } while (0)

int main()
{

    X( coo[0].kay() );
    X( coo[1].kay() );
    X( coo[2].kay() );
    X( koo[0].kay() );
    X( koo[1].kay() );
    X( koo[2].kay() );
    if ( 0x3f != x ) abort ();

    X( foo.kay() );
    if ( 0x7f != x ) abort ();

    X( goo.kay() );
    if ( 0xff != x ) abort ();

    X( xoo[0].kay() );
    X( xoo[1].kay() );
    X( xoo[2].kay() );
    X( zoo[0].kay() );
    X( zoo[1].kay() );
    X( zoo[2].kay() );
    if ( 0x3fff != x ) abort ();

    X( doo[0].kay() );
    X( doo[1].kay() );
    X( doo[2].kay() );
    X( hoo[0].kay() );
    X( hoo[1].kay() );
    X( hoo[2].kay() );
    if ( 0xfffff != x ) abort ();

    exit (0);
}
