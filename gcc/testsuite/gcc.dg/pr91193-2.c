/* Test ICE using function name in parameter type in old-style function
   definition (bug 91193).  */
/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */

typedef int T;

void
f (void)
{
  void T (x) T x; { } /* { dg-error "expected declaration specifiers|defaults to 'int'" } */
}
