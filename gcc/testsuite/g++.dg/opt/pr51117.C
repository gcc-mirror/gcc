// PR tree-optimization/51117
// { dg-do compile }
// { dg-options "-O2 -fexceptions" }

struct A { char buf[64]; };
void bar (A *);

int
foo ()
{
  A c;
  bar (&c);
  try
  {
    {
      A a;
      bar (&a);
      if (a.buf[13])
        throw 1;
      else if (a.buf[52])
        throw 3;
    }
    {
      A b;
      bar (&b);
      if (b.buf[13])
        throw 2;
    }
  }
  catch ( ...)
  {
    return 1;
  }
  return 0;
}

// { dg-final { scan-assembler-not "__cxa_rethrow" } }
