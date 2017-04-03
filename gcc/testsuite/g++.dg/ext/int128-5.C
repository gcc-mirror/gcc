// PR c++/79896
// { dg-do compile { target { ilp32 && { ! int128 } } } }
// { dg-options "" }

enum E
{
  e1 = 0xffffffffffffffffULL,
  e2,			// { dg-error "overflow in enumeration values" }
  e3
} e = e3;
