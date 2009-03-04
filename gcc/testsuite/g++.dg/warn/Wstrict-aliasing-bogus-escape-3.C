/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing" } */

struct Node_base {};

struct Node : Node_base
{
  int data;
};

struct List
{
  Node_base node, *prev;

  List() : prev(&node) { xyz(); }

  void xyz();

  int back() { return static_cast<Node*>(prev)->data; }
};

struct A
{
  virtual ~A();
};

A* foo();

void bar()
{
  List y;
  if (y.back())
    delete foo();
}

