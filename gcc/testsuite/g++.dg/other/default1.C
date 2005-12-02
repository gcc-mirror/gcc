// PR c++/24103
// ICE in simple_cst_equal
// Origin: Alexander Stepanov <astepanov@softminecorp.com>
// { dg-do compile }
// { dg-options "" }

struct S
{
  int i;
};

struct A
{
  A(S = (S){0});
};

struct B
{
  B(S = (S){0});
};

B::B(S) {}
