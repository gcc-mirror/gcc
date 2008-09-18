// { dg-do assemble  }
// { dg-options "-Wconversion" } 
// Test that we resolve this case as mandated by the standard, but also
// warn about it.  We choose op char* not because it is a member of B --
// the standard says that all conversion ops are treated as coming from
// the type of the argument -- but because it is non-const.

struct A  {
  operator const char *() const { return ""; }
};

struct B : public A {
  operator char *() { return 0; }
};

int main()
{
  B b;
  if ((const char *)b != 0)  // { dg-warning "choosing 'B" "B" } surprising overload resolution
  // { dg-warning "for conversion" "conv" { target *-*-* } 19 }
  // { dg-message "note" "note" { target *-*-* } 19 }
    return 1;
  if ((const char *)(const B)b == 0)
    return 2;
  if ((const char *)(const B &)b == 0)
    return 3;
}
