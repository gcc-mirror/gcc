// PR c++/20153

template <typename T>
void
foo()
{
  union { struct { }; }; // { dg-error "" }
}
