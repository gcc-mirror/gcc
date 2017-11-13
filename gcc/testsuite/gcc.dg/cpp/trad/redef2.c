/* Test for redefining traditional macros with significant differences.  */

/* { dg-do preprocess } */

#define foo bar    /* { dg-message "-:previous def" "foo prev def" } */
#define foo barr   /* { dg-warning "-:redefined" "foo redefined" } */

#undef foo
#define foo bar    /* { dg-message "-:previous def" "foo prev def 2" } */
#define foo() bar    /* { dg-warning "-:redefined" "foo redefined 2" } */

#undef foo
#define foo() bar    /* { dg-message "-:previous def" "foo prev def" } */
#define foo() barr   /* { dg-warning "-:redefined" "foo redefined" } */

#define quux(thud) a thud b /* { dg-message "-:previous def" "quux prev def" } */
#define quux(thu) a thud b   /* { dg-warning "-:redefined" "quux redefined" } */

#define bar(x, y) x+y /* { dg-message "-:previous def" "bar prev def" } */
#define bar(x, y) x+x   /* { dg-warning "-:redefined" "bar redefined" } */

#define bat(x, y) x+y  /* { dg-message "-:previous def" "bat prev def" } */
#define bat(x, y) x+ y   /* { dg-warning "-:redefined" "bat redefined" } */

#define baz(x, y) x+y  /* { dg-message "-:previous def" "baz prev def" } */
#define baz(x, y) x +y   /* { dg-warning "-:redefined" "baz redefined" } */

#define f(x, y) "x y"  /* { dg-message "-:previous def" "f prev def" } */
#define f(x, y) "x  y"   /* { dg-warning "-:redefined" "f redefined" } */

#define g(x, y) 'x'  /* { dg-message "-:previous def" "g prev def" } */
#define g(x, y) ' x'   /* { dg-warning "-:redefined" "g redefined" } */
