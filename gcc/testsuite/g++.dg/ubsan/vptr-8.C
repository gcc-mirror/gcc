// { dg-do run }
// { dg-shouldfail "ubsan" }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=vptr" }

extern "C" void abort ();

struct S { virtual void f () {} };
struct T : S { ~T (); };
struct U : S { };
struct V : T, virtual U {};

U *up;
V *vp;

int
main ()
{
  V v;
  up = vp = &v;
}

T::~T ()
{
  if (vp != up)
   abort ();
}

// { dg-output "\[^\n\r]*vptr-8.C:24:\[0-9]*: runtime error: cast to virtual base of address 0x\[0-9a-fA-F]* which does not point to an object of type 'V'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'T'(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?vptr for 'T'" }
