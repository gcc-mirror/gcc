// PR sanitizer/80110
// { dg-do compile }
// { dg-options "-fnon-call-exceptions -fsanitize=thread" }

struct A
{
  int b ();
  void c () { static int d = b (); }
};

void
foo ()
{
  A e;
  e.c ();
}
