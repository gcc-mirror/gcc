// PR c++/52953
// { dg-do compile }
// { dg-options "-pedantic-errors -Wno-switch-unreachable" }

void
foo (int x)				// { dg-message "'int x' previously declared here" }
{
  int x;				// { dg-error "declaration of 'int x' shadows a parameter" }
}

void
bar (int x)				// { dg-message "'int x' previously declared here" }
try
{
  int x;				// { dg-error "declaration of 'int x' shadows a parameter" }
}
catch (...)
{
}

volatile int v;

void
baz ()
{
#if __cplusplus >= 201103L
  auto f = [] (int x) { int x; };	// { dg-error "declaration of 'int x' shadows a parameter" "" { target c++11 } }
					// { dg-message "'int x' previously declared here" "" { target c++11 } .-1 }
#endif
  if (int x = 1)			// { dg-message "'int x' previously declared here" }
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  if (int x = 0)			// { dg-message "'int x' previously declared here" }
    ;
  else
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  if (int x = 1)			// { dg-message "'int x' previously declared here" }
    int x;				// { dg-error "redeclaration of 'int x'" }
  if (int x = 0)			// { dg-message "'int x' previously declared here" }
    ;
  else
    int x;				// { dg-error "redeclaration of 'int x'" }
  switch (int x = 1)			// { dg-message "'int x' previously declared here" }
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    default:;
    }
  switch (int x = 1)			// { dg-message "'int x' previously declared here" }
    int x;				// { dg-error "redeclaration of 'int x'" }
  while (int x = v)			// { dg-message "'int x' previously declared here" }
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  while (int x = v)			// { dg-message "'int x' previously declared here" }
    int x;				// { dg-error "redeclaration of 'int x'" }
  for (int x = v; x; ++x)		// { dg-message "'int x' previously declared here" }
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  for (int x = v; x; ++x)		// { dg-message "'int x' previously declared here" }
    int x;				// { dg-error "redeclaration of 'int x'" }
  for (; int x = v; )			// { dg-message "'int x' previously declared here" }
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  for (; int x = v; )			// { dg-message "'int x' previously declared here" }
    int x;				// { dg-error "redeclaration of 'int x'" }
  try
    {
    }
  catch (int x)				// { dg-message "'int x' previously declared here" }
    {
      int x;				// { dg-error "redeclaration of 'int x'" }
    }
  if (int x = 1)
    if (int x = 1)
      ;
  if (int x = 0)
    ;
  else
    if (int x = 1)
      ;
  if (int x = 1)
    switch (int x = 1)
      ;
  if (int x = 0)
    while (int x = v)
      ;
  if (int x = 0)
    for (int x = v; x; ++x)
      ;
  switch (int x = 1)
    switch (int x = 1)
      {
      case 1:;
      }
  while (int x = 0)
    if (int x = 1)
      ;
  for (int x = v; x; ++x)
    for (int x = v; x; ++x)
      ;
}

void
qux (int x)				// { dg-message "'int x' previously declared here" }
try
{
}
catch (int x)				// { dg-error "declaration of 'int x' shadows a parameter" }
{
}

void
corge (int x)				// { dg-message "'int x' previously declared here" }
try
{
}
catch (...)
{
  int x;				// { dg-error "declaration of 'int x' shadows a parameter" }
}

void
fred (int x)				// { dg-message "'int x' previously declared here" }
try
{
}
catch (int)
{
}
catch (long)
{
  int x;				// { dg-error "declaration of 'int x' shadows a parameter" }
}

void
garply (int x)
{
  try
    {
      int x;
    }
  catch (...)
    {
      int x;
    }
}

struct S
{
  S (int x)				// { dg-message "'int x' previously declared here" }
  try : s (x)
  {
    int x;				// { dg-error "declaration of 'int x' shadows a parameter" }
  }
  catch (...)
  {
  }
  int s;
};

struct T
{
  T (int x)				// { dg-message "'int x' previously declared here" }
  try : t (x)
  {
  }
  catch (...)
  {
    int x;				// { dg-error "declaration of 'int x' shadows a parameter" }
  }
  int t;
};

struct U
{
  U (int x) : u (x)
  {
    try
    {
      int x;
    }
    catch (...)
    {
      int x;
    }
  }
  int u;
};

struct V
{
  V (int x) : v (x)
  {
    {
      int x;
    }
  }
  int v;
};

void
foobar (int x)
try
{
}
catch (int)
{
  try
  {
  }
  catch (int x)
  {
  }
  try
  {
  } catch (int)
  {
    int x;
  }
}
