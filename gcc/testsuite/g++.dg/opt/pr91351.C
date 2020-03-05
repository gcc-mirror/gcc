// PR tree-optimization/91351
// { dg-do run }
// { dg-options "-O2 -fstrict-enums" }

enum E { e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12,
	 e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25 };

__attribute__((noipa)) void
foo ()
{
  __builtin_abort ();
}

__attribute__((noipa)) void
bar ()
{
}

__attribute__((noipa)) void
baz (E e)
{
  switch (e)
    {
    case e11:
    case e12:
    case e13: foo (); break;
    case e24: break;
    case e14:
    case e15: break;
    default: bar (); break;
    }
}

int
main ()
{
  baz (e3);
}
