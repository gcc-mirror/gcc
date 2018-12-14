// PR c++/87436
// { dg-do compile { target { c++11 && size32plus } } }
// { dg-options "-O2 -fdump-tree-gimple" }

// Verify we don't "optimize" the ctor as copying a 384MB .rodata
// object into the variable.  It is better to initialize it through
// two nested loops.
// { dg-final { scan-tree-dump-not "this->arr = " "gimple" } }

struct S {
  int a = -1;
  short b = 3;
  int x = 0;
  int y = 1;
  int z = 42;
  float f = 0.123f;
};

struct T { S arr[4096][4096]; };

T *
foo ()
{
  return new T;
}
