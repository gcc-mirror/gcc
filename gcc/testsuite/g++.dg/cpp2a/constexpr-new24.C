// PR c++/118775
// { dg-do compile { target c++20 } }

int a;

constexpr char *
f1 ()
{
  constexpr auto p = new char[(long int) &a]; // { dg-error "size not constant" }
  return p;
}

constexpr char *
f2 ()
{
  auto p = new char[(long int) &a];  // { dg-error "size not constant" }
  return p;
}

void
g ()
{
  auto r1 = f2 ();
  constexpr auto r2 = f2 (); // { dg-message "in .constexpr. expansion" }
}
