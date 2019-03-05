// { dg-do compile { target c++17 } }
// { dg-options "" }
// Test C++17 selection statements with initializer, basic use.

extern int foo (void);
extern void bar (int);
extern int g;

void
f (void)
{
  if (auto p = foo (); p > 10)
    bar (p);
  else
    bar (-p);

  if ((g += 2); g > 6)
    bar (1);

  if (auto a = 9, b = foo (); a + b > 10)
    bar (a + b);
  else
    bar (a - b);

  if (({ int a; 1;}))
    bar (0);

  if (auto i = foo (); i > 6)
    bar (0);
  else if (i++; i > 8)
    bar (1);
}

extern void lock (void);

void
f2 (int i)
{
  if (lock (); i > 10)
    ++i;
  else
    --i;
}

void
f3 (int i)
{
  switch (i *= 2; auto idx = i)
    {
    case 4:
      bar (3);
      break;
    default:
      break;
    }
}

void
f4 (void)
{
  if constexpr (constexpr auto s = sizeof (int); s > 10)
    foo ();
}
