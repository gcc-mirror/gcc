/* { dg-additional-options "-fdump-tree-gimple" }  */

/* PR c++/118486  */

struct NotAnInt {};

// Wrong return type:
NotAnInt var1();
#pragma omp declare variant(var1) match(user={condition(true)})
int base1();
/* { dg-error "variant 'NotAnInt var1\\(\\)' and base 'int base1\\(\\)' have incompatible types" "" { target *-*-* } .-2 } */


// Wrong return type:
NotAnInt var2();
float var2(float);
#pragma omp declare variant(var2) match(user={condition(true)})
int base2();
/* { dg-error "variant 'NotAnInt var2\\(\\)' and base 'int base2\\(\\)' have incompatible types" "" { target *-*-* } .-2 } */


// OK:
NotAnInt var3();
#pragma omp declare variant(var3) match(user={condition(true)})
NotAnInt base3();

void f()
{
  // int x;
  NotAnInt y;

  //x = base1 ();
  //x = base2 ();
  y = base3 ();
}

/* { dg-final { scan-tree-dump "var3 \\(\\);" "gimple" } }  */
/* { dg-final { scan-tree-dump-not "base3" "gimple" } }  */
