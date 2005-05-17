// PR middle-end/21492
// { dg-do compile }
// { dg-options "-Os -fPIC" }

extern char *bar (const char *, const char *);
extern char *baz (char *, const char *);
extern unsigned int fn (const char *);
static const struct C { int i; } k = { 0};

struct A
{
  ~A ();
};

char *
foo (char *x, const char *y)
{
  A a;
  char *c = x;

  if (bar (y, "foo"))
    {
      baz (c, "foo");
      c += fn ("foo");
    }
  else if (bar (y, "bar"))
    {
      baz (c, "bar");
      c += fn ("bar");
    }

  return x;
}
