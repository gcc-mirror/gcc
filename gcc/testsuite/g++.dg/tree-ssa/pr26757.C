// PR c++/26757
// { dg-do run }
// { dg-options "-O" }

extern "C" void abort ();

typedef struct A
{
  int c;
  int d;
} A;

A *b;

void
foo ()
{
  b->c++;
  extern A *b;
  b->d++;

}

void
bar ()
{
  if (b->d)
    b->c++;
}


int
main ()
{
  A a = { 0, 0 };
  b = &a;
  foo ();
  bar ();
  if (b->c != 2)
    abort ();
  if (b->d != 1)
    abort ();
  return 0;
}
