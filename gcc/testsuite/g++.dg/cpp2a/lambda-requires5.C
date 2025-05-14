// PR c++/109961
// { dg-do compile { target c++20 } }

auto a = requires{
    []( int b ) consteval {
       if( b ) {
            throw b;
       }
    }( 0 );
};
