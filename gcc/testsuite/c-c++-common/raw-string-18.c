/* PR preprocessor/57824 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-std=gnu99 -fdump-tree-optimized-lineno" { target c } } */
/* { dg-options "-fdump-tree-optimized-lineno" { target c++ } } */

const char x[] = R"(
abc
def
ghi
)";

int
main ()
{
  extern void foo (); foo ();
  return 0;
}

/* Verify call to foo is on line 15.  */
/* { dg-final { scan-tree-dump "c:15:\[^\n\r\]*foo" "optimized" } } */
