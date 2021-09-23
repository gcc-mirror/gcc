/* PR c++/74765 - missing uninitialized warning (parenthesis,
   TREE_NO_WARNING abuse)
   { dg-do compile }
   { dg-options "-Wall" } */

int warn_equal_parens (int x, int y)
{
  int i;

  if ((i == 0))     // { dg-warning "\\\[-Wuninitialized" }
    return x;

  return y;
}

int warn_equal (int x, int y)
{
  int i;

  if (i == 0)       // { dg-warning "\\\[-Wuninitialized" }
    return x;

  return y;
}
