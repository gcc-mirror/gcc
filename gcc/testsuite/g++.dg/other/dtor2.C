/* PR c++/35317 */
/* { dg-do "compile" } */

struct A
{
  void operator delete[] (void*, ...);
};
