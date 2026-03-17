/* PR c/123472 */
/* { dg-do compile } */
/* { do-options "-O2" } */

int a , b , c ;
__attribute__ ( ( __noinline__ ) ) int fn1 ( ) {
if ( ( b | ( a != ( a & c ) ) ) == 1 )
__builtin_abort ( ) ;
return 0 ;
}
int c ( void ) {  /* { dg-error "redeclared as different kind" } */
a = 5 ;
c = 1 ; /* { dg-error "lvalue required" } */
b = 6 ;
return fn1 ( ) ;
}
