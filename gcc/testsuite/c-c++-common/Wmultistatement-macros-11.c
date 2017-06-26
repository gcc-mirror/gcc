/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

enum E { A, B };

const char *
foo (enum E e)
{
#define CASE(X) case X: return #X
  switch (e)
    {
      CASE (A);
      CASE (B);
    default:
      return "<unknown>";
    }
#undef CASE
};
