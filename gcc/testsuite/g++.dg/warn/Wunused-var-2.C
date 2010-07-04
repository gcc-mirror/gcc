// { dg-options "-Wunused -W" }

extern void foo ();

void
f1 ()
{
  try
    {
      foo ();
    }
  catch (int i)
    {
    }
  catch (double d)
    {
    }
}

void
f2 (int x)
{
  int a = 0;
  x++;
  ++a;
}

struct A
{
  bool foo () const { return true; }
};

int
f3 ()
{
  A a;
  bool b = a.foo ();
  return b;
}

struct B
{
  int i;
  B (int j);
};

void
f4 ()
{
  B b (6);
}

struct C
{
  int i;
  C (int j) : i (j) {}
};

void
f5 ()
{
  C c (6);
}

struct D
{
  int i;
  D (int j) : i (j) {}
  ~D ();
};

void
f6 ()
{
  D d (6);
}

int *f7 (int s)
{
  return new int[s];
}

template <typename T>
T *f8 (int s)
{
  return new T[s];
}

template int *f8<int> (int);

void
f9 (char *p)
{
  delete p;
}

template <typename T>
void
f10 (T *p)
{
  delete p;
}

template void f10<char> (char *);
