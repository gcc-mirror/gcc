/* { dg-lto-do link } */
/* { dg-lto-options {{-fcompare-debug -flto} {-fcompare-debug -fwhopr}} } */

struct S {
    virtual void f() { }
};

int main(int, char *[])
{
  S s;
  return 0;
}
