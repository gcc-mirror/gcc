// { dg-options -std=c++1z }

extern int foo (void);
extern void bar (int), die (void);

void
f (void)
{
  if (auto i = foo (); i != -1)
    bar (1);
  else
    die ();

  i = 10; // { dg-error "not declared" }
}

void
f2 (void)
{
  switch (auto i = foo (); i)
    {
    case 0:
      bar (i + 1);
      break;
    case 1:
      bar (i + 10);
      break;
    default:
      break;
    }

  i = 10; // { dg-error "not declared" }
}

void
f3 (void)
{
  if constexpr (constexpr auto i = sizeof (long); i < 2)
    die ();
  i = 4; // { dg-error "not declared" }
}


void
f4 (void)
{
  {
    if (auto i = foo (); i > -1)
      {
	if (i > 5)
	  bar (i);
	if (auto j = foo (); true)
	  j++;
	j--; // { dg-error "not declared" }
      }
    i = 10; // { dg-error "not declared" }
  }
  i = 10; // { dg-error "not declared" }
}
// { dg-options -std=c++1z }

extern int foo (void);
extern void bar (int), die (void);

void
f (void)
{
  if (auto i = foo (); i != -1)
    bar (1);
  else
    die ();

  i = 10; // { dg-error "not declared" }
}

void
f2 (void)
{
  switch (auto i = foo (); i)
    {
    case 0:
      bar (i + 1);
      break;
    case 1:
      bar (i + 10);
      break;
    default:
      break;
    }

  i = 10; // { dg-error "not declared" }
}

void
f3 (void)
{
  if constexpr (constexpr auto i = sizeof (long); i < 2)
    die ();
  i = 4; // { dg-error "not declared" }
}


void
f4 (void)
{
  {
    if (auto i = foo (); i > -1)
      {
	if (i > 5)
	  bar (i);
	if (auto j = foo (); true)
	  j++;
	j--; // { dg-error "not declared" }
      }
    i = 10; // { dg-error "not declared" }
  }
  i = 10; // { dg-error "not declared" }
}
