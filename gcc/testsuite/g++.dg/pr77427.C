// { dg-do compile }

void bar (__builtin_va_list &);

struct c
{
  operator const __builtin_va_list &();
  operator __builtin_va_list &();
};

void
foo (void)
{
  struct c c1;

  bar (c1);
}
