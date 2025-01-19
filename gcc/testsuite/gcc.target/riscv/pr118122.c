/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ter -fno-forward-propagate" } */
char c;

void
foo(short s)
{
  s += 34231u;
  c = s;
}


