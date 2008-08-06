/* PR middle-end/35432 */

struct A
{
  char c[0];
};

void foo(struct A a)
{
  (a = a).c;
}
