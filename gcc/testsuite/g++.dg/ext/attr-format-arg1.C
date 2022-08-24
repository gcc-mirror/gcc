// PR c++/101833
// { dg-do compile }
// { dg-options "-Wall" }

struct B { };

struct V : virtual B {
  const char *fmt (int, const char *) __attribute__((format_arg(3)));
};

struct D : B {
  const char *fmt (int, const char *) __attribute__((format_arg(3)));
};

extern void fmt (const char *, ...) __attribute__((format(printf, 1, 2)));

void
g ()
{
  V v;
  fmt (v.fmt (1, "%d"), 1);
  fmt (v.fmt (1, "%d"), 1lu); // { dg-warning "expects argument of type" }
  D d;
  fmt (d.fmt (1, "%d"), 1);
  fmt (d.fmt (1, "%d"), 1lu); // { dg-warning "expects argument of type" }
}
