/* { dg-do link } */
/* { dg-xfail-if "" { *-*-* } { "*" } { "" } }  See PR34743.  */
/* { dg-options "-O2" } */

struct A
{
  int x;
  float y;
};

volatile float X, Y;

int __attribute__ ((__noinline__))
baz (struct A *z, struct A *y)
{
  z->x = (int) X;
  z->y = Y;
  y->x = (int) X;
  y->y = Y;
}


struct A B;

float foo (int i)
{
  struct A *p, x, y, z;

  p = (i > 10) ? &x : &z;
  x.y = 3.0;
  p->x += baz (&z, &y);
  X = z.y;
  Y = p->y;

  /* This predicate should always evaluate to false.  The call to
     baz() is not a clobbering site for x.y.  The operand scanner was
     considering it a clobbering site for x.y because x.y is in the
     alias set of a call-clobbered memory tag.  */
  if (x.y != 3.0)
    link_error ();
}

main(int argc, char **argv)
{
  foo (argc);
}
