// { dg-options "-Wsign-compare" }

int foo()
{
  unsigned char b = '1';

  bool x = ~b; // { dg-bogus "promoted bitwise complement of an unsigned value is always nonzero.*promoted bitwise complement of an unsigned value is always nonzero" }
 // { dg-warning "promoted bitwise complement of an unsigned value is always nonzero" "" { target *-*-* } .-1 }

  return 0;
}
