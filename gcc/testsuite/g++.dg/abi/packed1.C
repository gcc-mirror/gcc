// PR c++/41788
// { dg-options "-Wpacked" }
// { dg-do run }

extern "C" void abort ();

struct INNER {
  virtual int foo() const { return 1; }
} __attribute__ ((packed));

struct OUTER {
  char c;
  INNER inner;
} __attribute__ ((packed));

int main()
{
  OUTER outer;
  int s = sizeof(outer);
  int o = (char *)&outer.inner - (char *)&outer;
  if (s != sizeof (char) + sizeof (void*)
      || o != sizeof (char))
    abort ();
}
