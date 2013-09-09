// PR c++/58325
// { dg-do compile }
// { dg-options "-Wunused" }

void
f1 ()
{
  int *volatile a = new int[1];
  delete[] a;
}

void
f2 ()
{
  int *b = new int[1];
  delete[] b;
}

void
f3 ()
{
  int *volatile c = new int;
  delete c;
}

void
f4 ()
{
  int *d = new int;
  delete d;
}
