/* { dg-do run } */ 
/* { dg-options "-O2 -std=gnu89" } */

struct A {
    int x;
    int y;
};

baz (struct A *a)
{
  a->x = 3;
  a->y = 2;
}

foo (int i)
{
  struct A a;

  /* Make sure we can't scalarize 'a'.  */
  baz (&a);

  if (i > 10)
    a.x = i;
  else
    a.x = i;

  /* Copy propagation should prove that this predicate is always false.  */
  if (a.x != i)
    link_error ();

  return a.x;
}

main ()
{
  foo (30);
  return 0;
}
