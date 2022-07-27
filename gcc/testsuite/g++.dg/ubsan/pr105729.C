// PR sanitizer/105729
// { dg-do run }
// { dg-options "-fsanitize=null -fno-sanitize-recover=null" }

int
foo (int x)
{
  throw 0;
}

struct S {};
struct T {
  S *data;
  T () : data (0) {}
  const S &bar (int x) const { return data[foo (x)]; }
};

int
main ()
{
  T t;
  try
    {
      t.bar (-1);
    }
  catch (...)
    {
    }
}
