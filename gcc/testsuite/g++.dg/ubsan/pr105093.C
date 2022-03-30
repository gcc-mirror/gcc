// PR sanitizer/105093
// { dg-do compile }
// { dg-options "-O2 -fsanitize=undefined -Wno-volatile" }

struct X { X (); ~X (); };

volatile X
foo ()
{
  X x;
  return x;
}
