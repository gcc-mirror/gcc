/* { dg-lto-do link } */
/* { dg-lto-options {{-fcompare-debug -flto -flto-partition=none} {-fcompare-debug -flto -flto-partition=1to1}} } */

struct S {
    virtual void f() { }
};

int main(int, char *[])
{
  S s;
  return 0;
}
