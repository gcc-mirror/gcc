// PR middle-end/36449
// { dg-do run }
// { dg-options "-O3" }

extern "C" void exit (int);
extern "C" void abort ();

struct R
{
  short a;
  short b;
};

struct S
{
  R e;
  long f;
  long g;
};

struct T
{
  short c;
  short d;
};

struct U
{
  long h[0x1ffffff + 1];
  T i;
};

U *j;

void __attribute__((noinline))
bar ()
{
  exit (0);
}

void __attribute__((noinline))
foo ()
{
  S s;

  s.e.a = 36;
  s.e.b = 38;
  if (s.e.a == j->i.c && s.e.b == j->i.d)
    bar ();
}

int
main ()
{
  try
    {
      j = new U;
    }
  catch (...)
    {
      return 0;
    }
  j->i.c = 36;
  j->i.d = 38;
  j->h[0] = 1;
  j->h[1] = 2;
  j->h[2] = 3;
  foo ();
  abort ();
}
