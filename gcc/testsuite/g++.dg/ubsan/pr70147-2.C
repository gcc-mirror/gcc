// PR c++/70147
// { dg-do run }
// { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } }
// { dg-options "-fsanitize=vptr" }

struct A
{
  A () : a (0) {}
  A (int x) : a (x) {}
  virtual void f () {}
  virtual int i () { int r = 0; __asm ("" : "+r" (r)); return r; }
  int a;
};
struct E
{
  E () : e (0) {}
  E (int x) : e (x) {}
  virtual void f () {}
  virtual int g () { int r = 0; __asm ("" : "+r" (r)); return r; }
  int e;
};
struct F
{
  F () : f (0) {}
  F (int x) : f (x) {}
  virtual int h () { int r = 0; __asm ("" : "+r" (r)); return r; }
  int f;
};
struct B : virtual A, public E, public F
{
  B ()
    : E (
         g ()
         + h ()
         + i ()
        ),
      F (g ()
         + h ()
         + i ()),
      b (g () + h () + i ())	// It is ok to call the methods here.
  {
    b += g () + h () + i ();	// And here too.
  }
  virtual void f () {}
  int b;
};
struct C : B, virtual A
{
  C () : A (i ()) {}
};

int
main ()
{
  C c;
}

// { dg-output "\[^\n\r]*pr70147-2.C:49:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'A'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?invalid vptr\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*pr70147-2.C:33:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'E'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?invalid vptr(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*pr70147-2.C:34:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'F'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?invalid vptr\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*pr70147-2.C:38:\[0-9]*: runtime error: member call on address 0x\[0-9a-fA-F]* which does not point to an object of type 'F'(\n|\r\n|\r)" }
// { dg-output "0x\[0-9a-fA-F]*: note: object has invalid vptr(\n|\r\n|\r)" }
// { dg-output "  ?.. .. .. ..  ?.. .. .. ..  ?.. .. .. .. \[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?\\^~~~~~~~~~~\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "              ?invalid vptr" }
