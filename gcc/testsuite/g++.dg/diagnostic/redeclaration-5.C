// PR c++/52953
// { dg-do compile }
// { dg-options "-pedantic-errors -Wno-switch-unreachable" }

void
foo (int x)				// { dg-message "'int x' previously declared here" }
{
  extern int x (int);			// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" }
}

void
bar (int x)				// { dg-message "'int x' previously declared here" }
try
{
  extern int x (int);			// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" }
}
catch (...)
{
}

volatile int v;

void
baz ()
{
#if __cplusplus >= 201103L
  auto f = [] (int x) { extern int x (int); };// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" "" { target c++11 } }
					// { dg-message "'int x' previously declared here" "" { target c++11 } .-1 }
#endif
  if (int x = 1)			// { dg-message "'int x' previously declared here" }
    {
      extern int x (int);		// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
    }
  if (int x = 0)			// { dg-message "'int x' previously declared here" }
    ;
  else
    {
      extern int x (int);		// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
    }
  if (int x = 1)			// { dg-message "'int x' previously declared here" }
    extern int x (int);			// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
  if (int x = 0)			// { dg-message "'int x' previously declared here" }
    ;
  else
    extern int x (int);			// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
  switch (int x = 1)			// { dg-message "'int x' previously declared here" }
    {
      extern int x (int);		// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
    default:;
    }
  switch (int x = 1)			// { dg-message "'int x' previously declared here" }
    extern int x (int);			// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
  while (int x = v)
    {
      extern int x (int);		// { dg-error "'int x\\\(int\\\)' redeclared as different kind of entity" }
    }
  while (int x = v)
    extern int x (int);			// { dg-error "'int x\\\(int\\\)' redeclared as different kind of entity" }
  for (int x = v; x; ++x)		// { dg-message "'int x' previously declared here" }
    {
      extern int x (int);		// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
    }
  for (int x = v; x; ++x)		// { dg-message "'int x' previously declared here" }
    extern int x (int);			// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
  for (; int x = v; )
    {
      extern int x (int);		// { dg-error "'int x\\\(int\\\)' redeclared as different kind of entity" }
    }
  for (; int x = v; )
    extern int x (int);			// { dg-error "'int x\\\(int\\\)' redeclared as different kind of entity" }
  try
    {
    }
  catch (int x)				// { dg-message "'int x' previously declared here" }
    {
      extern int x (int);		// { dg-error "redeclaration of 'int x\\\(int\\\)'" }
    }
}

void
corge (int x)				// { dg-message "'int x' previously declared here" }
try
{
}
catch (...)
{
  extern int x (int);			// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" }
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
  extern int x (int);			// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" }
}

void
garply (int x)
{
  try
    {
      extern int x (int);
    }
  catch (...)
    {
      extern int x (int);
    }
}

struct S
{
  S (int x)				// { dg-message "'int x' previously declared here" }
  try : s (x)
  {
    extern int x (int);			// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" }
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
    extern int x (int);			// { dg-error "declaration of 'int x\\\(int\\\)' shadows a parameter" }
  }
  int t;
};

struct U
{
  U (int x) : u (x)
  {
    try
    {
      extern int x (int);
    }
    catch (...)
    {
      extern int x (int);
    }
  }
  int u;
};

struct V
{
  V (int x) : v (x)
  {
    {
      extern int x (int);
    }
  }
  int v;
};
