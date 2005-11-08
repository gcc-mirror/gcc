/* { dg-do compile } */
/* { dg-options "-O2" } */

enum eumtype { ENUM1, ENUM2 };
void g(const eumtype kind );
void f(long i);
void g(const eumtype kind)
{
  if ((kind != ENUM1) && (kind != ENUM2))
    f(kind);
}
