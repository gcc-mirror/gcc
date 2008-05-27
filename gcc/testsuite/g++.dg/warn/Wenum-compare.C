/* Test that we get the -Wenum-compare by default.  See PR27975.  */
/* { dg-do compile } */
/* { dg-options "" } */
enum E1 { a };
enum E2 { b };

int foo (E1 e1, E2 e2)
{
  return e1 == e2;  /* { dg-warning "comparison between" } */
}
