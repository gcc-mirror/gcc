// { dg-do run }
// { dg-shouldfail "ubsan" }
// { dg-options "-fsanitize=vptr -fno-sanitize-recover=undefined" }

struct S { virtual int f () { return 0; } };
struct T : virtual S {};
struct U { virtual int f () { return 0; } };

int
main ()
{
  U u;
  T *t = (T *) &u;
  S *s = t;
  return s->f ();
}

// { dg-output "\[^\n\r]*vptr-9.C:14:\[0-9]*: runtime error: cast to virtual base of address 0x\[0-9a-fA-F]* which does not point to an object of type 'T'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object is of type 'U'(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?vptr for 'U'" }
