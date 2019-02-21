// PR c++/84192
// { dg-do compile { target c++14 } }
// { dg-options "" }

bool
f1 ()
{ 
  return ({ return true; }) && false;	// { dg-error "could not convert" }
}

void
f2 ()
{ 
  for (;;)
    constexpr bool b = ({ break; false; }) && false;	// { dg-error "is not a constant expression" }
}

constexpr bool
f3 (int n)
{
  bool b = false;
  for (int i = 0; i < n; i++)
    b = ({ break; });	// { dg-error "void value not ignored as it ought to be" }
  return b;
}

constexpr bool b = f3 (4);

bool
f4 ()
{
  constexpr bool b = ({ return true; }) && false;	// { dg-error "could not convert" }
  return false;
}

constexpr bool
f5 (int x)
{
  constexpr bool b = ({ switch (x) case 0: true; }) && false;	// { dg-error "could not convert" }
  return false;
}
