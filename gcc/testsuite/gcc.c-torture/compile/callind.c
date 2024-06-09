/* { dg-require-effective-target indirect_calls } */
/* { dg-additional-options "-std=gnu89" } */

bar (foo, a)
     int (**foo) ();
{

  (foo)[1] = bar;

  foo[a] (1);
}
