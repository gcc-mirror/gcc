// PR c++/89948
// { dg-options "" }

void f ()
{
  int a = 0;
  for (;;)
    for (;a=+({break;1;});)
      {}
}
