// PR c++/20148
// { dg-options "" }

void foo()
{
  if (({int c[2];})) ; // { dg-error "\{\.\.\.\}" }
}

void bar()
{
  if (({})); // { dg-error "\{\.\.\.\}" }
}
