// PR c++/98043
// { dg-do compile { target c++11 } }

enum class B { A };
struct C { B c : 8; };

bool
foo (C x)
{
  switch (x.c)
    {
    case B::A:
      return false;
    default:
      return true;
    }
}

enum E { X };
struct D { E c : 7; };

bool
bar (D x)
{
  switch (x.c)
    {
    case E::X:
      return false;
    default:
      return true;
    }
}
