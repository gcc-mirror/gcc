// { dg-do run  }
// Bug: a ends up in the text segment, so trying to initialize it causes
// a seg fault.

struct A {
  int i;
  A(): i(0) {}
  A(int j): i(j) {}
};

const A a;
const A b(1);

int main ()
{
  return 0;
}
