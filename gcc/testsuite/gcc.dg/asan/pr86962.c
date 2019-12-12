/* PR sanitizer/86962 */
/* { dg-do compile } */

extern int dummy (int *);

void foo(int i)
{
  int j=i;

  void bar() { int x=j, y=i; }

  dummy(&i);
}
