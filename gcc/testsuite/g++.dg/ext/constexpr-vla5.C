// PR c++/84559
// { dg-do compile { target c++11 } }

void foo(int i)
{
  constexpr char x[i] = "";	// { dg-error "" }
}
