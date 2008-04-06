/* { dg-do compile } */
/* { dg-options "-Wtype-limits" } */

struct A
{
  A();
  ~A();
};

void foo()
{
  A x[1];
}
