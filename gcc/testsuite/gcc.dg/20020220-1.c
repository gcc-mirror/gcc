/* PR c++/4574
   This testcase ICEd because expand_and did not handle VOIDmode
   CONST_DOUBLE arguments.  */
/* { dg-do compile } */
/* { dg-options "-w" } */

struct A {
  unsigned long long b : 8;
  unsigned long long c : 18;
};

int main()
{
  struct A a;
  long long l;

  l = a.c = 0x123456789aULL;
  return 0;
}
