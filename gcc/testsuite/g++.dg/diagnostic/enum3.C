// PR c++/100879
// { dg-additional-options -Werror=sign-compare }

enum e1 { e1val };
enum e2 { e3val };

int main( int, char * [] ) {
   if ( e1val == e3val ) return 1; // { dg-warning -Wenum-compare }
}
