/* PR middle-end/55130 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O1 -mregparm=3 -mpreferred-stack-boundary=2" } */

extern void bar(long long);

int foo(long long a, char b, long long c, long long d)
{
  if (c == 0)
    c = d;

  bar(b + c);

  return a == d;
}
