// PR c++/20153

template <typename T>
void
foo()
{
  union { struct { }; }; // { dg-error "prohibits anonymous struct" "anon" }
  // { dg-error "not inside" "not inside" { target *-*-* } 7 }
}
