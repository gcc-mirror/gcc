/* { dg-lto-do run } */
/* { dg-lto-options {{-O2 -flto -flto-partition=none}} } */

struct bar {int x;};
extern struct bar foo(void);
int main()
{
  struct bar x=foo();
  return 0;
}

