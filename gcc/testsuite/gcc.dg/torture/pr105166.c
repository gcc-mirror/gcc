/* { dg-do compile } */

int bar (foo, a)
  int (**foo) ();
  int a;
{
  (foo)[1] = bar;
  foo[1] (1);
}
