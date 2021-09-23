/* PR c++/74762 - missing uninitialized warning (C++, parenthesized expr)
   { dg-do compile }
   { dg-options "-Wall" } */

struct tree2;
struct tree_vector2
{
  tree2 *elts[1];
};

struct tree2
{
  struct
  {
    tree_vector2 vector;
  } u;
};

tree2 *
const_with_all_bytes_same (tree2 *val)
{
  int i;
  return ((val->u.vector.elts[i]));   // { dg-warning "\\\[-Wuninitialized" }
}
