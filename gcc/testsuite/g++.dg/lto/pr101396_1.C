enum A : __UINT64_TYPE__ { // { dg-lto-note "6: an enum with different value name is defined in another translation unit" }
  // { dg-lto-note "6: a type with different precision is defined in another translation unit" "" { target *-*-* } .-1 }
  a, // { dg-lto-note "3: mismatching definition" }
  b,
  c
};

int f(enum A x)
{
  return (int) x;
}
