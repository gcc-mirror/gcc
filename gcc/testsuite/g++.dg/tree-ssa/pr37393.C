// PR middle-end/37393
// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  ~A ();
  bool foo () const;
};

extern "C"
{
  extern void bar (const char *, ...) __attribute__ ((noreturn));
  extern inline __attribute__ ((always_inline, gnu_inline, artificial)) void
  baz (const char *fmt, ...)
  {
    bar (fmt, __builtin_va_arg_pack ());
  }
};

void
test ()
{
  A a;
  if (a.foo ())
    baz ("foo");
}
