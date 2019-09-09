/* { dg-require-effective-target indirect_calls } */

bar (foo, a)
     int (**foo) ();
{

  (foo)[1] = bar;

  foo[a] (1);
}
