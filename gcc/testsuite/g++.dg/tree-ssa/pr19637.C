/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom1" } */

#include <new>

struct Foo {
  Foo() { i[0] = 1; }
  int i[2];
};

int foo_char(void)
{
  int i[2];
  new (reinterpret_cast<char *>(i)) Foo();
  return reinterpret_cast<Foo *>(i)->i[0];
}

int foo_void(void)
{
  int i[2];
  new (reinterpret_cast<void *>(i)) Foo();
  return reinterpret_cast<Foo *>(i)->i[0];
}

int foo_void_offset(void)
{
  int i[2];
  new (reinterpret_cast<void *>(&i[0])) Foo();
  return reinterpret_cast<Foo *>(&i[0])->i[0];
}

/* { dg-final { scan-tree-dump-times "return 1;" 3 "dom1" } } */
/* { dg-final { cleanup-tree-dump "dom1" } } */
