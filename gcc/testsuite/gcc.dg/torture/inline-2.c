/* { dg-do link } */

extern inline void foo2 (void) __attribute__((always_inline,gnu_inline));
extern inline void foo1 (void) __attribute__((always_inline,gnu_inline));
void bar1 (void);
void bar2 (void);

extern inline void __attribute__((always_inline,gnu_inline))
foo2 (void)
{
  bar2 ();
}

void
bar1 (void)
{
  foo2 ();
}

void
bar2 (void)
{
  foo1 ();
}

extern inline void __attribute__((always_inline,gnu_inline))
foo1 (void)
{
  bar1 ();
}

int main()
{
  return 0;
}
