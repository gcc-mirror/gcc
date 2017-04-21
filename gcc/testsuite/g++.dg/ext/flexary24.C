// PR c++/80179
// { dg-options "" }

struct S {
  int n;
  const char *a[];
};

void bar (const char *a)
{
  static const S t = { 1, { a, "b" } };
}
