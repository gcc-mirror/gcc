// PR 7944
// { dg-do compile }
// { dg-options -O2 }

struct B
{
  B & operator << (short s)
  {
    int j;
    if (j)
        return operator << (s);
    else
        return operator << (s);
  }
};

struct A
{
  int i;
  static void bar ();
  static int quux ()
  {
    bar ();
    return 0;
  }

  A ():i (quux ())
  {
  }
  ~A ()
  {
  }
};

void
foo ()
{
  short s[4] = { 0, 0, 0, 1 };
  A a[2] = { A (), A () };

  B b;
  b << s[0] << s[2];
}
