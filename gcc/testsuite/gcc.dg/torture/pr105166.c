/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

int bar (foo, a)
  int (**foo) ();
  int a;
{
  (foo)[1] = bar;
  foo[1] (1);
}
