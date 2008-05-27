/* Test disabling -Wenum-compare (on by default).  See PR27975.  */
/* { dg-do compile } */
/* { dg-options "-Wno-enum-compare" } */
enum E1 { a };
enum E2 { b };

int foo (E1 e1, E2 e2)
{
  return e1 == e2;  /* { dg-bogus "comparison between" } */
}
