// PR c++/65592
// { dg-do compile { target c++11 } }

struct S
{
  enum class C { };
};

void
bar (S::C)
{
}

void
foo ()
{
  S f;
  bar (f.C::X);  // { dg-error "13:'X' is not a member" }
}
