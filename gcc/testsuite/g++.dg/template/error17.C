// PR c++/20153

template <typename T>
void
foo()
{
  union { struct { }; }; // { dg-error "prohibits anonymous struct" "anon" }
  // { dg-error "18:anonymous struct not inside" "not inside" { target *-*-* } .-1 }
}
