// PR c++/51912
// { dg-do compile { target c++11 } }

constexpr double g() { return 2.0; }

void f(int i)
{
  switch (i)
    {
    case 1.0:;    // { dg-error "could not convert|conversion from" }
    }

  switch (i)
    {
    case g():;    // { dg-error "could not convert|conversion from" }
    }
}
