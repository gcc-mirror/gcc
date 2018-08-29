/* Test that we correctly complain about an invalid number of macro
   arguments.  */

/* { dg-do preprocess } */

#define f(x) x
#define g(x, y) x y
#define h()

f(); 		/* { dg-bogus "-:requires 1" "no arg is 1 empty arg" } */
f( ); 		/* { dg-bogus "-:macro" "1 arg to 1 param macro" } */
f(1,); 		/* { dg-error "-:passed 2" "2 args to 1 param macro" } */
f(1,2);		/* { dg-error "-:passed 2" "2 args to 1 param macro" } */
h();		/* { dg-bogus "-:macro" "no arg to 1 param macro" } */
h( );		/* { dg-error "-:passed 1" "1 arg to 0 param macro" } */
h(1,2);		/* { dg-error "-:passed 2" "2 args to 0 param macro" } */
g();		/* { dg-error "-:requires 2" "0 args to 2 param macro" } */
g( );		/* { dg-error "-:requires 2" "1 args to 2 param macro" } */
g( ,2);		/* { dg-bogus "-:requires 2" "2 args to 2 param macro" } */
g(,);		/* { dg-bogus "-:requires 2" "2 args to 2 param macro" } */
g(1,2,3);	/* { dg-error "-:passed 3" "3 args to 2 param macro" } */
