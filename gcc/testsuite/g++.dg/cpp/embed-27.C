// PR c++/122302
// { dg-do run { target c++11 } }
// { dg-options "-O2" }

unsigned char b[] = {
#embed "embed-27.C"
};

struct A {
  unsigned char a[sizeof (b)] = { 
#embed "embed-27.C"
  };
};

void
foo ()
{
  A a;
  for (int i = 0; i < sizeof (b); ++i)
    if (a.a[i] != b[i])
      __builtin_abort ();
}

void
bar ()
{
  A a;
  for (int i = 0; i < sizeof (b); ++i)
    if (a.a[i] != b[i])
      __builtin_abort ();
}

int
main ()
{
  foo ();
  bar ();
}
