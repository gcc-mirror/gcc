// PR middle-end/101586

struct A { char a; };
struct B : virtual A {};
struct C : B {};
struct D : virtual A, C {};

__attribute__((noipa)) A *
baz (C *p, D *q)
{
  if (p)
    return dynamic_cast <A *> (p);
  else
    return dynamic_cast <A *> (q);
}

void
foo ()
{ 
  C c;
  c.a = 42;
  __builtin_clear_padding (&c);
  A *p = baz (&c, 0);
  if (c.a != 42 || p->a != 42)
    __builtin_abort ();
}

void
bar ()
{
  D d;
  d.a = 42;
  __builtin_clear_padding (&d);
  A *p = baz (0, &d);
  if (d.a != 42 || p->a != 42)
    __builtin_abort ();
}

int
main ()
{
  foo ();
  bar ();
}
