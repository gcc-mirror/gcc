/* PR opt/14288 */
/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

volatile int sink;
extern int foo(int);

struct S
{
  int x;

  S() { x = foo(0); }
  ~S() { sink = x; }
};

int test(bool p)
{
  return p ? foo(S().x) : 0;	/* { dg-bogus "uninitialized" } */
}
