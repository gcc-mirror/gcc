// { dg-do run  }
// prms-id: 6311

struct Foo {
  int member;
} a = { 42 }, *ptra = &a;

int Foo::*pmd = &Foo::member;

int main() {
  if (pmd == 0)
    return 1;
  if (a.*pmd != 42)
    return 2;
  if (ptra->*pmd != 42)
    return 3;
}
