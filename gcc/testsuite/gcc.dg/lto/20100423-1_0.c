/* { dg-lto-do run } */

struct bar {int x;};
extern struct bar foo(void);
int main()
{
  struct bar x=foo();
  return 0;
}

