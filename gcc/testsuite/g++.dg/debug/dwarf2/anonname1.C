// PR debug/41828
// { dg-do compile { target c++11 } }
// { dg-options "-gdwarf-2 -dA" }
// { dg-final { scan-assembler-not "<anonymous" } }
// { dg-final { scan-assembler-not "\\._\[0-9\]" } }
// { dg-final { scan-assembler-not "\$_\[0-9\]" } }
// { dg-final { scan-assembler-not "__anon_" } }

struct
{
  union
  {
    struct
    {
      enum { a, b, c } x;
    } s;
  };
} x;
